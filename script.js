const RED = "#FF6666"
const GREEN = "#B2FF66" 
const LBLUE = "#E8F1FF"

const GOESTO = "\n\n\u25B9* "

var cc_status = 0
var dcc_status = 0

// cc_check_ctx : string -> string
// [cc_check_ctx(ctx)] checks if the ctx is well-formed.
// If so, it returns an empty string. If not, it returns the error message.
function cc_wf_button() {
    var envbox = document.getElementById("cc_env");
    var env = "[" + envbox.value + "]";

    var result = cc_check_ctx(env);

    var outbox = document.getElementById("cc_type");
    if (cc_status == 0) {
        outbox.style.backgroundColor = GREEN;
        outbox.value = result;
    } else {
        outbox.style.backgroundColor = RED;
        outbox.value = "Well-formedness check:\n" + result;
    }
}

// cc_infer : string*2 -> string
// [cc_infer(env, term)] gives the type of the term (normalized), or the error message if fails.
function cc_infer_button() {
	var envbox = document.getElementById("cc_env");
    var termbox = document.getElementById("cc_term");
    var typebox = document.getElementById("cc_type");

    // Get context
    var env = "[" + envbox.value + "]";

    var result = cc_infer(env, termbox.value);
    if (cc_status == 0) {
        cc_type.style.backgroundColor = LBLUE;
        typebox.value = result; 
    } else {
        cc_type.style.backgroundColor = RED;
        typebox.value = "Infer type:\n" + result;
    }   
}

// cc_normalize : string*2 -> string
// [cc_normalize(env, term)] normalizes the term, or gives the error message if it fails.
var cc_buffer = ""
function cc_norm_button() {
    var termbox = document.getElementById("cc_term");

    cc_buffer = termbox.value;
    back_button1.style.display = "block";

    var result = cc_normalize(termbox.value);
    if (cc_status == 0) {
        cc_term.style.backgroundColor = LBLUE;
        termbox.value = termbox.value + GOESTO + result; 
    } else {
        cc_term.style.backgroundColor = RED;
        termbox.value = "Normalize:\n" + result;
    }  
}

function cc_back_button() {
    document.getElementById("cc_term").value = cc_buffer;
    cc_term.style.backgroundColor = LBLUE;
    back_button1.style.display = "none";
}

// cc_check : string*3 -> bool
// [cc_check(env, term, type)] checks if env |- term : type.
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

// Clears the CC output box
function cc_clear() {
	document.getElementById("cc_type").value = "";
	cc_type.style.backgroundColor = LBLUE;
    cc_status = 0;
}

// dcc_check_lbctx : string -> string
// [dcc_check_lbctx(ctx)] checks if the label ctx is well-formed.
// If so, it returns an empty string. If not, it returns the error message.
function dcc_wflb_button() {
    var labbox = document.getElementById("dcc_lab_env");
    var defs = "[" + labbox.value + "]";

    var result = dcc_check_lbctx(defs);

    var outbox = document.getElementById("dcc_type");
    if (dcc_status == 0) {
        outbox.style.backgroundColor = GREEN;
        outbox.value = result;
    } else {
        outbox.style.backgroundColor = RED;
        outbox.value = "Label context well-formedness check:\n" + result;
    }
}

// dcc_check_ctx : string*2 -> string
// [dcc_check_ctx(defs, ctx)] checks if the type ctx is well-formed.
// If so, it returns an empty string. If not, it returns the error message.
function dcc_wf_button() {
    var labbox = document.getElementById("dcc_lab_env");
    var envbox = document.getElementById("dcc_env");
    
    var env = "[" + envbox.value + "]";
    var defs = "[" + labbox.value + "]";

    var result = dcc_check_ctx(defs, env);

    var outbox = document.getElementById("dcc_type");
    if (dcc_status == 0) {
        outbox.style.backgroundColor = GREEN;
        outbox.value = result;
    } else {
        outbox.style.backgroundColor = RED;
        outbox.value = "Type context well-formedness check:\n" + result;
    }
}

// dcc_infer: string*3 -> string
// [dcc_infer(type_env, label_env, term)]
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
        dcc_type.style.backgroundColor = LBLUE;  
    } else {
        dcc_type.style.backgroundColor = RED;
    }
}

// dcc_normalize : string*2 -> string
// [dcc_normalize(type_env, term)]
var dcc_buffer = ""
function dcc_norm_button() {
    var labbox = document.getElementById("dcc_lab_env");
    var termbox = document.getElementById("dcc_term");

    var lab = "[" + labbox.value + "]";

    dcc_buffer = termbox.value;
    back_button2.style.display = "block";

    var result = dcc_normalize(lab, termbox.value);
    if (dcc_status == 0) {
        dcc_term.style.backgroundColor = LBLUE;
        termbox.value = termbox.value + GOESTO + result; 
    } else {
        dcc_term.style.backgroundColor = RED;
        termbox.value = "Normalize:\n" + result;
    }  
}

function dcc_back_button() {
    document.getElementById("dcc_term").value = dcc_buffer;
    cc_term.style.backgroundColor = LBLUE;
    back_button2.style.display = "none";
}

// dcc_check: string*4 -> string
// [dcc_check(type_env, label_env, term, type)]
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
    dcc_type.style.backgroundColor = LBLUE;
    dcc_status = 0;
}

// transform: string*3 -> {defs, env, type, term, err : string}
function to_dcc() {
    var envbox = document.getElementById("cc_env");
    var termbox = document.getElementById("cc_term");
    var typebox = document.getElementById("cc_type");

    var env = "[" + envbox.value + "]";

    // Transformation is only defined for well-typed terms.
    // Checks if the input term is well-typed.

    var type = cc_infer(env, termbox.value);
    if (cc_status != 0){
        cc_type.style.backgroundColor = RED;
        typebox.value = "Transformation: cannot transform ill-typed terms\n" + type;
        return
    }

    // If there is an input type, check if it matches the inferred type.
    // If not, does not transform, but hint that the type can be inferred.
    if ((typebox.value != "") && (!cc_check(env, termbox.value, typebox.value))) {
        cc_type.style.backgroundColor = RED;
        typebox.value = 
            "Transformation: input type does not match the inferred type of input term\n" 
            + "Did you mean this type?\n" + type;
        return
    }

    if (typebox.value == "") {
        typebox.value = type;
    } else {
        type = typebox.value;
    }

    var result = transform(env, termbox.value, type);

    if (dcc_status == 0) {
        cc_type.style.backgroundColor = LBLUE;
        dcc_type.style.backgroundColor = LBLUE;
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

// back_transform: string*4 -> {env, type, term, err : string}
function to_cc() {
    var labbox = document.getElementById("dcc_lab_env");
    var envbox = document.getElementById("dcc_env");
    var termbox = document.getElementById("dcc_term");
    var typebox = document.getElementById("dcc_type");

    var env = "[" + envbox.value + "]";
    var defs = "[" + labbox.value + "]";

    // Transformation is only defined for well-typed terms.
    // Checks if the input term is well-typed.

    var type = dcc_infer(defs, env, termbox.value);
    if (dcc_status != 0){
        dcc_type.style.backgroundColor = RED;
        typebox.value = "Back transformation: cannot transform ill-typed terms\n" + type;
        return
    }

    // If there is an input type, check if it matches the inferred type.
    // If not, does not transform, but hint that the type can be inferred.
    if ((typebox.value != "") && (!dcc_check(defs, env, termbox.value, typebox.value))) {
        dcc_type.style.backgroundColor = RED;
        typebox.value = 
            "Back transformation: input type does not match the inferred type of input term\n" 
            + "Did you mean this type?\n" + type;
        return
    }

    if (typebox.value == "") {
        typebox.value = type;
    } else {
        type = typebox.value;
    }

    var result = back_transform(defs, env, termbox.value, type);
    if (dcc_status == 0) {
        cc_type.style.backgroundColor = LBLUE;
        dcc_type.style.backgroundColor = LBLUE;
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

function load_cc(env, term, type) {
    // load example texts
    document.getElementById("cc_env").value = env;
    document.getElementById("cc_term").value = term;
    document.getElementById("cc_type").value = type;

    // clear dcc texts
    document.getElementById("dcc_lab_env").value = "";
    document.getElementById("dcc_env").value = "";
    document.getElementById("dcc_term").value = "";
    document.getElementById("dcc_type").value = "";
}


