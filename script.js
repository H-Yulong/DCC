// External functions:

// cc_infer : string*2 -> string
// [cc_infer(env, term)] gives the type of the term, or the error message if fails.

// cc_check : string*3 -> bool
// [cc_check(env, term, type)] checks if env |- term : type.

// The external functions for DCC are similar.

// dcc_infer: string*3 -> string
// [dcc_infer(type_env, label_env, term)]

// dcc_check: string*4 -> string
// [dcc_check(type_env, label_env, term, type)]

function cc_infer_button() {
	var envbox = document.getElementById("cc_env");
    var termbox = document.getElementById("cc_term");
    var typebox = document.getElementById("cc_type");

    // Get context
    var env = "[" + envbox.value + "]";

    typebox.value = cc_infer(env, termbox.value);
    cc_type.style.backgroundColor = "white";
}

function cc_check_button() {
	var envbox = document.getElementById("cc_env");
    var termbox = document.getElementById("cc_term");
    var typebox = document.getElementById("cc_type");

    // Get context
    var env = "[" + envbox.value + "]";

    var result = cc_check(env, termbox.value, typebox.value);
    console.log(result);
    if (result) {
    	cc_type.style.backgroundColor = "#B2FF66";
    } else {
    	cc_type.style.backgroundColor = "#FF6666";
    }

}


