# Label Resolution

This is the first pass of the semantic analysis stage. Many of the C statements used for control flow require different parts of the program to be labeled, so the correct jump instructions can be generated in the next compiler pass.
`if` statements are handled by the next stage because they are relatively simple, but `goto`, `switch`, and loops are all handled here.

## Goto Resolution

This stage does two passes. 
For each function, it creates a list of all labels used in the function. Duplicate labels in the same function are rejected, but unique names are generated for each label so other functions can use the same names for labels. 

The second pass finds all `goto` statements. It ensures the label to jump to exists in the list of valid labels, and if so rewrites the label with the unique name generated for it earlier.

## Loop Resolution

The loop resolution stage generates a unique label for each loop, and then assigns a label to every `break` and `continue` statement corresponding to the loop enclosing it. This is needed so that in nested loops, a `break` statement only jumps out of the innermost loop.

This stage also does some of the work for resolving `switch` statements. 
If a `continue` is seen outside any loop, or a `break` is seen outside a loop or switch, and error is raised. 
This stage also checks that `case` and `default` statements are only used inside switch statements, ensures that `case` statements only use constant values, and generates unique names for `switch` statements.
Lastly, this stage labels return statements with the name of their enclosing function.
This tells the typechecking stage what return type the return expression needs to be cast to.

## Switch Resolution

This stage finds all `case` and `default` statements within a `switch` block. 
This allows the next stage to generate code that checks for each of the cases and jumps to the correct one.
This pass also ensures that there are no duplicate cases.