module.exports = grammar({
    name: 'COBOL',
    rules: {
        source_file: $ => seq(
            $.identification_division
            , optional($.environment_division)
        ),
        _user_defined_word: $ => /[A-Za-z][A-Za-z0-9_-]*/,
        program_name: $ => $._user_defined_word,
        comment_entry: $ =>
            /.*/,
        // identification division is optional in MF
        identification_division: $ => seq(
            "IDENTIFICATION DIVISION." // or "ID DIVISION." in MF OSVS and VSC2
            , seq(
                "PROGRAM-ID."
                , field("program_name",$.program_name)
                , optional(
                    //  ans85
                    choice(
                        "IS COMMON PROGRAM"
                        , "IS INITIAL PROGRAM"
                    )
                    // "IS EXTERNAL PROGRAM" MF
                )
            )

            , optional(
                seq("AUTHOR.", field("author", $.comment_entry))
            )
            , optional(seq("INSTALLATION.", field("installation", $.comment_entry)))
            , optional(seq("DATE-WRITTEN.", field("date_written", $.comment_entry)))
            , optional(seq("DATE-COMPILED.", field("date_compiled", $.comment_entry)))
            , optional(seq("SECURITY.", field("security", $.comment_entry)))
            // only in OSVS where it is optional
            // + "REMARKS. comment-entry"
        ),
        environment_division: $ => seq("ENVIRONMENT DIVISION.")
    }
});