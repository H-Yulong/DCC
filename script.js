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
// transform: string*3 -> {defs, env, type, term, err : string}
// back_transform: string*4 -> {env, type, term, err : string}

const RED = "#FF6666"
const GREEN = "#B2FF66" 

function cc_infer_button() {
	var envbox = document.getElementById("cc_env");
    var termbox = document.getElementById("cc_term");
    var typebox = document.getElementById("cc_type");

    // Get context
    var env = "[" + envbox.value + "]";

    typebox.value = cc_infer(env, termbox.value);
    if (cc_status == 0) {
        cc_type.style.backgroundColor = "white";  
    } else {
        cc_type.style.backgroundColor = RED;
    }
    
}

function cc_check_button() {
	var envbox = document.getElementById("cc_env");
    var termbox = document.getElementById("cc_term");
    var typebox = document.getElementById("cc_type");

    // Get context
    var env = "[" + envbox.value + "]";

    var result = cc_check(env, termbox.value, typebox.value);
    if (result) {
    	cc_type.style.backgroundColor = GREEN;
    } else {
    	cc_type.style.backgroundColor = RED;
    }

}

function cc_clear() {
	document.getElementById("cc_type").value = "";
	cc_type.style.backgroundColor = "white";
    cc_status = 0;
}

function dcc_infer_button() {
    var labbox = document.getElementById("dcc_lab_env");
    var envbox = document.getElementById("dcc_env");
    var termbox = document.getElementById("dcc_term");
    var typebox = document.getElementById("dcc_type");

    // Get context
    var env = "[" + envbox.value + "]";
    var defs = "[" + labbox.value + "]";

    typebox.value = dcc_infer(defs, env, termbox.value);
    if (dcc_status == 0) {
        dcc_type.style.backgroundColor = "white";  
    } else {
        dcc_type.style.backgroundColor = RED;
    }
}

function dcc_check_button() {
    var labbox = document.getElementById("dcc_lab_env");
    var envbox = document.getElementById("dcc_env");
    var termbox = document.getElementById("dcc_term");
    var typebox = document.getElementById("dcc_type");

    // Get context
    var env = "[" + envbox.value + "]";
    var defs = "[" + labbox.value + "]";

    var result = dcc_check(defs, env, termbox.value, typebox.value);
    if (result) {
        dcc_type.style.backgroundColor = GREEN;
    } else {
        dcc_type.style.backgroundColor = RED;
    }

}

function dcc_clear() {
    document.getElementById("dcc_type").value = "";
    dcc_type.style.backgroundColor = "white";
    dcc_status = 0;
}

function to_dcc() {
    var envbox = document.getElementById("cc_env");
    var termbox = document.getElementById("cc_term");
    var typebox = document.getElementById("cc_type");

    var env = "[" + envbox.value + "]";
    var type = typebox.value

    // Check if the term input is well-typed when typebox is blank
    // Do not transform if ill-typed and display error message
    if ((type == "") || (cc_status != 0)) {
        type = cc_infer(env, termbox.value);
        if (cc_status != 0) {
            cc_type.style.backgroundColor = RED;
            typebox.value = type;
            return
        }
    }

    var result = transform(env, termbox.value, type);

    if (dcc_status == 0) {
        dcc_type.style.backgroundColor = "white";
        document.getElementById("dcc_lab_env").value = result[1];
        document.getElementById("dcc_env").value = result[2];
        document.getElementById("dcc_term").value = result[3];
        document.getElementById("dcc_type").value = result[4];
    } else {
        dcc_type.style.backgroundColor = RED;
        document.getElementById("dcc_lab_env").value = "";
        document.getElementById("dcc_env").value = "";
        document.getElementById("dcc_term").value = "";
        document.getElementById("dcc_type").value = result[5];
    }
}

function to_cc() {
    var labbox = document.getElementById("dcc_lab_env");
    var envbox = document.getElementById("dcc_env");
    var termbox = document.getElementById("dcc_term");
    var typebox = document.getElementById("dcc_type");

    var env = "[" + envbox.value + "]";
    var defs = "[" + labbox.value + "]";
    var type = typebox.value
    if (type == "") {
        type = dcc_infer(env, termbox.value);
    }

    var result = back_transform(defs, env, termbox.value, type);
    if (cc_status == 0) {
        cc_type.style.backgroundColor = "white";
        document.getElementById("cc_env").value = result[1];
        document.getElementById("cc_term").value = result[2];
        document.getElementById("cc_type").value = result[3];
    } else {
        cc_type.style.backgroundColor = RED;
        document.getElementById("cc_env").value = "";
        document.getElementById("cc_term").value = "";
        document.getElementById("cc_type").value = result[4];
    }
}


