module.exports = grammar({
    name: 'COBOL',

    rules: {
        source_file: $ => $.identification_division,
        // identification division is optional in MF
        identification_division: $ => (
            "IDENTIFICATION DIVISION." // or "ID DIVISION." in MF OSVS and VSC2
            + "PROGRAM-ID. program-name"
            // 'is _ program' are optional
            // "IS COMMON PROGRAM" ans85
            // "IS INITIAL PROGRAM" ans85
            // "IS EXTERNAL PROGRAM" MF
            // optional
            + "AUTHOR. comment-entry"
            // optional
            + "INSTALLATION. comment-entry"
            // optional
            + "DATE-WRITTEN. comment-entry"
            // optional
            + "DATE-COMPILED. comment-entry"
            // optional
            + "SECURITY. comment-entry"
            // only in OSVS where it is optional
            // + "REMARKS. comment-entry"
        )
    }
});