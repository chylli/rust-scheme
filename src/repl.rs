use std::ffi::CString;
use std::ffi::CStr;
use std::os::raw::c_char;

#[link(name = "readline")]
extern "C"{
    fn readline(prompt: *const c_char) -> *const c_char;
    fn add_history(entry: *const c_char);
}

fn prompt_for_input(prompt: &str) -> Option<String> {
    let prompt_c_str = CString::new(prompt).unwrap();
    // wait for enter/CTRL-C/CTRL-D
    let raw = unsafe{ readline(prompt_c_str.as_ptr())};
    if raw.is_null(){ return None}
    let cs =unsafe { CStr::from_ptr(raw)};
    // parse into String and return
    let string = cs.to_str().unwrap().to_owned();
    if string.len() > 0 {
        //add to shell history
        unsafe{ add_history(raw)};
    }
    Some(string)
}

pub fn start<F: Fn(String) -> Result<String, String>>(prompt: &str, f: F ) {
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
