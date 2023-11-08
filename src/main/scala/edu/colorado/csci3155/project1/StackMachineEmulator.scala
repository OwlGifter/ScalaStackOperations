package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack and a single instruction of type StackMachineInstruction
        Return a stack that results when the instruction is executed from the stack.
        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.

     */
    def emulateSingleInstruction(stack: List[Double], ins: StackMachineInstruction): List[Double] = ins match {
        case AddI => 
            if (stack.size < 2) throw new RuntimeException("not big enough")
            else (stack(0) + stack(1)) :: stack.drop(2)
        case SubI => 
            if (stack.size < 2) throw new RuntimeException("not big enough")
            else (stack(1) - stack(0)) :: stack.drop(2)
        case MultI => 
            if (stack.size < 2) throw new RuntimeException("not big enough")
            else (stack(0) * stack(1)) :: stack.drop(2)
        case DivI => 
            if (stack.size < 2) throw new RuntimeException("not big enough")
            if (stack(0) == 0) throw new RuntimeException("cant divide by zero")
            else (stack(1) / stack(0)) :: stack.drop(2)
        case ExpI => 
            if (stack.isEmpty) throw new RuntimeException("Empty")
            else math.exp(stack.head) :: stack.tail
        case LogI => 
            if (stack.isEmpty) throw new RuntimeException("Empty")
            if (stack.head <= 0) throw new RuntimeException("Log of 0 or less")
            else math.log(stack.head) :: stack.tail
        case SinI => 
            if (stack.isEmpty) throw new RuntimeException("Empty")
            else math.sin(stack.head) :: stack.tail
        case CosI => 
            if (stack.isEmpty) throw new RuntimeException("Empty")
            else math.cos(stack.head) :: stack.tail
        case PushI(f) => f :: stack
        case PopI => 
            if (stack.isEmpty) throw new RuntimeException("Empty")
            else stack.tail
    }
    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Double ={
        val FS = instructionList.foldLeft(List.empty[Double])((S, ins) => {
            emulateSingleInstruction(S, ins)
        })
        if (FS.isEmpty) throw new RuntimeException("Empty")
        FS.head
    }
}