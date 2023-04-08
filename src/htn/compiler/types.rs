use std::rc::Rc;

pub struct Node {
    name: Rc<str>,
    parent: Rc<Node>
}