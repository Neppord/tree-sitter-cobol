module.exports = grammar({
    name: 'COBOL',

    rules: {
        source_file: $ => seq(
            $.identification_division
        ),
        // identification division is optional in MF
        identification_division: $ => seq(
            "IDENTIFICATION DIVISION." // or "ID DIVISION." in MF OSVS and VSC2
            , seq(
                "PROGRAM-ID. program-name"
                , optional(
                    //  ans85
                    choice(
                        "IS COMMON PROGRAM"
                        , "IS INITIAL PROGRAM"
                    )
                    // "IS EXTERNAL PROGRAM" MF
                )
            )

            , optional("AUTHOR. comment-entry")
            , optional("INSTALLATION. comment-entry")
            , optional("DATE-WRITTEN. comment-entry")
            , optional("DATE-COMPILED. comment-entry")
            , optional("SECURITY. comment-entry")
            // only in OSVS where it is optional
            // + "REMARKS. comment-entry"
        )
    }
});