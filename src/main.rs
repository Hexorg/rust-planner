
mod pddl;
use pddl::{
    domain::Domain,
    expression::Expression,
    action::Action,
    problem::Problem
};

fn main() {
    let mut d = Domain::new();
    let p_room = d.new_predicate("ROOM", 1);
    let p_ball = d.new_predicate("BALL", 1);
    let p_at_robby = d.new_predicate("at-robby", 1);
    let p_at_ball = d.new_predicate("at-ball", 2);
    let p_gripper = d.new_predicate("GRIPPER", 1);
    let p_free = d.new_predicate("free", 1);
    let p_carry = d.new_predicate("carry", 2);

    let room_x = Expression::Value(&d.predicates[p_room], vec![0]);
    let room_y = Expression::Value(&d.predicates[p_room], vec![1]);
    let at_robby_x = Expression::Value(&d.predicates[p_at_robby], vec![0]);
    let at_robby_y = Expression::Value(&d.predicates[p_at_robby], vec![1]);
    let not_at_robby_x = expNot!(at_robby_x);
    let precondition = expAnd![room_x, room_y, at_robby_x];
    let effect = expAnd![at_robby_y, not_at_robby_x];

    d.actions.push(Action{name:"move".to_string(), precondition, effect});

    let ball_x = Expression::Value(&d.predicates[p_ball], vec![0]);
    let gripper_z = Expression::Value(&d.predicates[p_gripper], vec![2]);
    let at_ball_x_y = Expression::Value(&d.predicates[p_at_ball], vec![0, 1]);
    let free_z = Expression::Value(&d.predicates[p_free], vec![2]);
    let precondition = expAnd![ball_x, room_y, gripper_z, at_ball_x_y, at_robby_y, free_z];
    let carry_z_x = Expression::Value(&d.predicates[p_carry], vec![2, 0]);
    let not_at_ball_x_y = expNot!(at_ball_x_y);
    let not_free_z = expNot!(free_z);
    let effect = expAnd![carry_z_x, not_at_ball_x_y, not_free_z];
    d.actions.push(Action{name:"pick-up".to_string(), precondition, effect});

    let precondition = expAnd![ball_x, room_y, gripper_z, carry_z_x, at_robby_y];
    let not_carry_z_x = expNot!(carry_z_x);
    let effect = expAnd![at_ball_x_y, free_z, not_carry_z_x];
    d.actions.push(Action{name:"drop".to_string(), precondition, effect});

    let objects = vec!["rooma", "roomb", "ball1", "ball2", "ball3", "ball4", "left", "right"];
    let room_rooma = Expression::Value(&d.predicates[p_room], vec![0]);
    let room_roomb = Expression::Value(&d.predicates[p_room], vec![1]);
    let ball_b1 = Expression::Value(&d.predicates[p_ball], vec![2]);
    let ball_b2 = Expression::Value(&d.predicates[p_ball], vec![3]);
    let ball_b3 = Expression::Value(&d.predicates[p_ball], vec![4]);
    let ball_b4 = Expression::Value(&d.predicates[p_ball], vec![5]);
    let gripper_left = Expression::Value(&d.predicates[p_gripper], vec![6]);
    let gripper_right = Expression::Value(&d.predicates[p_gripper], vec![7]);
    let free_left = Expression::Value(&d.predicates[p_free], vec![6]);
    let free_right = Expression::Value(&d.predicates[p_free], vec![7]);
    let at_robby_rooma = at_robby_x;
    let at_ball_b1_ra = Expression::Value(&d.predicates[p_at_ball], vec![2, 0]);
    let at_ball_b2_ra = Expression::Value(&d.predicates[p_at_ball], vec![3, 0]);
    let at_ball_b3_ra = Expression::Value(&d.predicates[p_at_ball], vec![4, 0]);
    let at_ball_b4_ra = Expression::Value(&d.predicates[p_at_ball], vec![5, 0]);
    let at_ball_b1_rb = Expression::Value(&d.predicates[p_at_ball], vec![2, 1]);
    let at_ball_b2_rb = Expression::Value(&d.predicates[p_at_ball], vec![3, 1]);
    let at_ball_b3_rb = Expression::Value(&d.predicates[p_at_ball], vec![4, 1]);
    let at_ball_b4_rb = Expression::Value(&d.predicates[p_at_ball], vec![5, 1]);

    let problem = Problem{
        objects: objects.iter().map(|x| x.to_string()).collect(),
        initial_state: expAnd![room_rooma, room_roomb, ball_b1, ball_b2, ball_b3, ball_b4, gripper_left, gripper_right, free_left, free_right, at_robby_rooma, at_ball_b1_ra, at_ball_b2_ra, at_ball_b3_ra, at_ball_b4_ra],
        goal: expAnd![at_ball_b1_rb, at_ball_b2_rb, at_ball_b3_rb, at_ball_b4_rb]
    };
    println!("{}\n{}", d, problem);
}
