use std::ffi::CString;
use std::ffi::CStr;
use std::os::raw::c_char;

#[link(name = "readline")]
extern "C"{
    fn readline(prompt: *const c_char) -> *const c_char;
}

fn prompt_for_input(prompt: &str) -> Option<String> {
    let prompt_c_str = CString::new(prompt).unwrap();
    let raw = unsafe { readline(prompt_c_str.as_ptr()) };
    let cs = unsafe { CStr::from_ptr(raw) };
    let string = cs.to_str().unwrap().to_owned();
    if string.len () == 0 {
        None
    } else {
        Some(string)
    }
}

pub fn start(prompt: &str, f: fn(String) -> Result<String, String>) {
    loop {
        match prompt_for_input(prompt) {
            Some(input) => {
                let result = f(input);
                println!("{}", result.unwrap_or_else(|e| e));
            },
            None => return
        };
    };
}
