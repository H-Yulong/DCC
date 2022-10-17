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

// We also have the transformation functions
// transform: string*3 -> {env: string, term: string, type: string}

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
    if (result) {
    	cc_type.style.backgroundColor = "#B2FF66";
    } else {
    	cc_type.style.backgroundColor = "#FF6666";
    }

}

function cc_clear() {
	document.getElementById("cc_type").value = "";
	cc_type.style.backgroundColor = "white";
}

function dcc_infer_button() {
    var envbox = document.getElementById("dcc_env");
    var termbox = document.getElementById("dcc_term");
    var typebox = document.getElementById("dcc_type");

    // Get context
    var env = "[" + envbox.value + "]";

    typebox.value = dcc_infer(termbox.value);
    dcc_type.style.backgroundColor = "white";
}

function dcc_check_button() {
    var envbox = document.getElementById("dcc_env");
    var termbox = document.getElementById("dcc_term");
    var typebox = document.getElementById("dcc_type");

    // Get context
    var env = "[" + envbox.value + "]";

    var result = dcc_check(termbox.value, typebox.value);
    if (result) {
        dcc_type.style.backgroundColor = "#B2FF66";
    } else {
        dcc_type.style.backgroundColor = "#FF6666";
    }

}

function dcc_clear() {
    document.getElementById("dcc_type").value = "";
    cc_type.style.backgroundColor = "white";
}

function to_dcc() {
    var envbox = document.getElementById("cc_env");
    var termbox = document.getElementById("cc_term");
    var typebox = document.getElementById("cc_type");

    var env = "[" + envbox.value + "]";
    var type = typebox.value
    if (type == "") {
        type = cc_infer(env, termbox.value);
    }

    var result = transform(env, termbox.value, type);
    document.getElementById("dcc_lab_env").value = result[1];
    document.getElementById("dcc_env").value = result[2];
    document.getElementById("dcc_term").value = result[3];
    document.getElementById("dcc_type").value = result[4];

}


