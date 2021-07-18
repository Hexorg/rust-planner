pub fn build_var_string(vars: &Vec<usize>, objects: Option<&Vec<String>>) -> String {
    let unnamed_objects = vec!["x", "y", "z", "p", "q", "j", "k", "t"];
    let unnamed_objects = unnamed_objects.iter().map(|x| x.to_string()).collect();
    let var_names = if let Some(objects) = objects {
        objects
    } else { 
        &unnamed_objects
    };
    let first = vars.iter().take(1).fold("?".to_string(), |acc,item| acc + &var_names[*item]);
    vars.iter().skip(1).fold(first, |acc,item| acc + " ?" + &var_names[*item])
}