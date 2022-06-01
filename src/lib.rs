pub mod htn;
use htn::{domain::Domain, planner::{Planner, State}};
use std::{os::raw::{c_char, c_int}, ffi::{CStr, CString}, mem};

// #[no_mangle]
// pub unsafe extern "C" fn load_domain(filepath: *const c_char) -> Box<Domain> {
//     let path = CStr::from_ptr(filepath).to_str().unwrap();
//     println!("Loading domain from {}", path);
//     Box::new(Domain::from_file(path).expect("Domain construction error"))
// }

// #[no_mangle]
// pub unsafe extern "C" fn plan(domain:Box<Domain>, outlen: *mut c_int) -> *mut *mut c_char {
//     let plan = Planner::run(&domain).unwrap();
//     let mut out = plan.into_iter().map(|s| CString::new(s).unwrap().into_raw()).collect::<Vec<_>>();
//     out.shrink_to_fit();
//     let len = out.len();
//     let ptr = out.as_mut_ptr();
//     mem::forget(out);
//     std::ptr::write(outlen, len as c_int);
//     ptr
// }