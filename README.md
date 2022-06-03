# Hierarchical Task Network planner

[AI Planning](https://en.wikipedia.org/wiki/Automated_planning_and_scheduling) is a process of realization of strategies or action sequences. Hierarchical Task Networks are a subset of AI Planning approaches where tasks(actions) can require calling other tasks or be composed of multiple methods. 

A task that is composed of multiple methods is called a composite task. A composite task is planned successfully when one of its methods is planned to run. A method and a primitive task can only call other tasks or operators. Operators are atomic identifiers of the underlying process and are represented by Strings. This project is inspired by [AI Pro's article on HTNs](https://www.gameaipro.com/GameAIPro/GameAIPro_Chapter12_Exploring_HTN_Planners_through_Example.pdf)

# Syntax

Many research projects in HTNs are driven by the LISP community (e.g. [SHOP2](https://github.com/cl-axon/shop2)) which often drives new developers away. This project implements a python-like syntax parser to allow for a friendlier HTN experience. Sample problems can be found in [htn-problems](htn-problems/) folder. Overall the syntax uses newlines and tabulation as means of introducing expression separators and blocks. The following keywords are recognized:

* `task` - represents the beginning of a task statement. Full syntax: `task NAME(preconditions):` where `NAME` is any non-keyword string of alphanumeric characters and `preconditions` is a boolean expression
* `method` - represents the beginning of a method statement. Methods can be defined inside `task`s only. Full syntax: `method NAME(preconditions):` where `NAME` is any non-keyword string of alphanumeric characters and `preconditions` is a boolean expression.
* `else` - syntactic sugar for `method DontNAME(!preconditions):` - inverts the preconditions of a previous method. Since this is an equivalent of a method, `else` can only appear in a `task` body.
* `effects` - represents the beginning of `task`'s effects block. Effects usually contain variable assignment statements. Must appear on the same identation level as `task` without any blank lines before the last task.
* `or`, `and`, `not` - syntactic sugar for `|`, `&`, `!` boolean operations.
* `true`, `false` - syntactic sugar for `1` and `0` literals. Internally all expressions operate on Rust's `i32` type. 

Example:

```
task Main:
    method FindTrunk(WsTrunkHealth == 0): # precondition - can only run this is WsTrunkHealth is 0
        UprootTrunk()
        Main() # Supports recursion, will plan Main task again. 
    method Attack(WsCanSeeEnemy): # name(conditions)
        AttackEnemy()
        Main()
    else: # syntactic sugar for "method DontAttack(!WsCanSeeEnemy)"
        CheckBridge()
        Main()
    method Wait:
        wait()
        Main()

task AttackEnemy:
    method AttackWithTrunk(WsTrunkHealth > 0):
        NavigateToEnemy()
        DoTrunkSlam()

task DoTrunkSlam(WsTrunkHealth > 0):
    DoTrunkSlamOperator()
effects:
    WsTrunkHealth -= 1
```