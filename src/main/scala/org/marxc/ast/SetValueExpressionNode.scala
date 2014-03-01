package org.marxc.ast

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._
import org.marxc.SymbolTable

case class SetValueExpressionNode(operand: OperandNode) extends ExpressionNode{
  def generate(mv: MethodVisitor, symbolTable: SymbolTable) {
    operand.generate(mv, symbolTable)
  }
}
