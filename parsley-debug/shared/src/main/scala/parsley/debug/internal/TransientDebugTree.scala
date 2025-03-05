/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.debug.internal

import scala.collection.mutable

import parsley.debug.{DebugTree, ParseAttempt}

/** A mutable implementation of [[DebugTree]], used when constructing the tree as a parser is
  * running.
  *
  * When viewing / analysing the parse tree, it is highly advised to call
  * [[TransientDebugTree#freeze]] to obtain a frozen, immutable version of the debug tree.
  *
  * @param name Name of parser.
  * @param parse What attempts to parse have been made?
  * @param children This debug tree node's children.
  */
private [parsley] class TransientDebugTree(var name: String = "", var internal: String = "", val fullInput: String,
                                           var parse: Option[ParseAttempt] = None, var cNumber: Option[Long] = None,
                                           val children: mutable.ListBuffer[TransientDebugTree] = mutable.ListBuffer.empty,
                                           var iterative: Boolean = false, var newlyGenerated: Boolean = true) extends DebugTree {
    // These are user-facing, and will depend heavily on what the parser looks like.
    // $COVERAGE-OFF$
    override def parserName: String = name

    override def internalName: String = internal

    override def childNumber: Option[Long] = cNumber

    // The pair stores the input the parser attempted to parse and its success.
    override def parseResults: Option[ParseAttempt] = parse

    override def nodeChildren: List[DebugTree] = children.toList

    
    // If the parser needs bubbling then we do not see it because it is transparent. 
    // If it does not need bubbling then it is either an iterative, opaque parser 
    // or a non-iterative, opaque parser.
    // To cover both cases, we also check if it is iterative.
      
    override def isIterative: Boolean = iterative

    override def isNewlyGenerated: Boolean = newlyGenerated

    // Factors out inputs or results for parsers with children.
    private type Augment  = (Long, (Int, Int))
    private var augmentId = 0L
    private val augments  = mutable.ListBuffer.empty[Augment]

    private [parsley] def augmentInput(startIndex: Int, endIndex: Int): Long = {
        augmentId += 1L
        val uuid = augmentId
        augments.append((uuid, (startIndex, endIndex)))
        uuid
    }

    private [parsley] def applyInputAugments(): Unit = {
        parse = parse.map { p =>
            // Augments are single-use.
            val ua = augments.toList
            augments.clear()

            def basis(n: Int): Int = n - p.fromOffset

            p.copy(inp = ua.foldRight(p.rawInput) {
                // don't augment input when the consumption was rolled-back
                case ((aid, (_, aen)), st) if aen > p.toOffset =>
                    // remove the UID from the child (ew gross, there has to be a better way to do this?!)
                    children.find(_.cNumber.contains(aid)).foreach { t =>
                        t.cNumber = None
                    }
                    st
                case ((aid, (ast, aen)), st) => s"${st.take(basis(ast))}{$aid}${st.drop(basis(aen))}"
            })
        }
    }

    // Make a copy with possibly different fields
    private [debug] def copy(name: String = name, internal: String = internal, fullInput: String = fullInput,
                                 parse: Option[ParseAttempt] = parse, cNumber: Option[Long] = cNumber,
                                 children: mutable.ListBuffer[TransientDebugTree] = children,
                                 iterative: Boolean = iterative, newlyGenerated: Boolean = newlyGenerated): TransientDebugTree
        = new TransientDebugTree(name, internal, fullInput, parse, cNumber, children, iterative)
        
    // Strips all `remoteBreak` nodes from the tree
    private [debug] def withoutBreakpoints(): TransientDebugTree = {
        val childrenWithoutBreak = children.map(_.withoutBreakpoints())
        if (internalName == "remoteBreak") {
            childrenWithoutBreak.head // Debug nodes *should* only ever have one and only one child
        } else new TransientDebugTree(name, internal, fullInput, parse, cNumber, childrenWithoutBreak, iterative)
    }

    private [debug] def resetNewlyGeneratedFlags(): Unit = {
        newlyGenerated = false
        children.foreach(_.resetNewlyGeneratedFlags())
    }

    private [debug] def withoutNewlyGeneratedFlags(): TransientDebugTree = {
        resetNewlyGeneratedFlags()
        this
    }
}
