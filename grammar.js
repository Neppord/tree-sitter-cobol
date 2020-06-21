module.exports = grammar({
  name: "COBOL",
  rules: {
    source_file: ($) =>
      seq(
        $.identification_division,
        // Environment division is optional in ans85
        optional($.environment_division)
      ),
    _user_defined_word: ($) => /[A-Za-z][A-Za-z0-9_-]*/,
    program_name: ($) => $._user_defined_word,
    comment_entry: ($) => /.*/,
    // identification division is optional in MF
    identification_division: ($) =>
      seq(
        "IDENTIFICATION DIVISION.", // or "ID DIVISION." in MF OSVS and VSC2
        seq(
          "PROGRAM-ID.",
          field("program_name", $.program_name),
          optional(
            //  ans85
            choice("IS COMMON PROGRAM", "IS INITIAL PROGRAM")
            // "IS EXTERNAL PROGRAM" MF
          )
        ),

        optional(seq("AUTHOR.", field("author", $.comment_entry))),
        optional(seq("INSTALLATION.", field("installation", $.comment_entry))),
        optional(seq("DATE-WRITTEN.", field("date_written", $.comment_entry))),
        optional(
          seq("DATE-COMPILED.", field("date_compiled", $.comment_entry))
        ),
        optional(seq("SECURITY.", field("security", $.comment_entry)))
        // only in OSVS where it is optional
        // + "REMARKS. comment-entry"
      ),
    environment_division: ($) =>
      seq(
        "ENVIRONMENT DIVISION.",
        optional($.configuration_section),
        // Input-Output section is optional for MF
        optional($.input_output_section)
      ),
    source_computer_entry: ($) => "source-computer-entry",
    object_computer_entry: ($) => "object-computer-entry",
    special_names_entry: ($) => "special-names-entry",
    configuration_section: ($) =>
      seq(
        "CONFIGURATION SECTION.",
        "SOURCE-COMPUTER.",
        field("source_computer", $.source_computer_entry),
        "OBJECT-COMPUTER.",
        field("object_computer", $.object_computer_entry),
        "SPECIAL-NAMES.",
        field("special_names", $.special_names_entry)
      ),
    _file_reference: ($) =>
      choice("external-file-reference", "data-name", "literal"),
    _external_or_dynamic: ($) => choice("EXTERNAL", "DYNAMIC"),
    file_control_entry: ($) =>
      choice($._record_sequential_file, $._relative_file),
    _record_sequential_file: ($) =>
      seq(
        "SELECT",
        optional("OPTIONAL"),
        "file-name",
        "ASSIGN TO",
        choice(
          seq(
            optional($._external_or_dynamic),
            choice(
              seq(
                choice(
                  "LINE ADVANCING",
                  seq("MULTIPLE", choice("REEL", "UNIT"))
                ),
                "FILE"
              ),
              "DISC",
              "KEYBOARD",
              "DISPLAY",
              "PRINTER",
              "PRINTER-1"
            ),
            $._file_reference
          ),
          seq("DISC", "FROM", "data-name")
        ),
        optional(seq("RESERVE", "integer-1", choice("AREA", "AREAS"))),
        optional(seq(optional("ORGANIZATION IS"), "SEQUENTIAL")),
        optional(seq("PADDING CHARACTER IS", choice("data-name", "literal"))),
        optional(
          seq("RECORD DELIMITER IS", choice("STANDARD-1", "character-string"))
        ),
        optional("ACCESS MODE IS SEQUENTIAL"),
        optional(seq("FILE STATUS IS", "data-name"))
      ),
    _relative_file: ($) =>
      seq(
        "SELECT",
        optional("OPTIONAL"),
        "file-name",
        "ASSIGN TO",
        choice(
          seq(optional($._external_or_dynamic), "DISK", $._file_reference),
          seq(optional($._external_or_dynamic)),
          seq("DISK FROM", "data-name")
        ),
        optional(seq("RESERVE", "integer", choice("AREA", "AREAS"))),
        seq(optional("ORGANIZATION"), "RELATIVE"),
        optional(
          seq(
            "ACCESS MODE IS",
            choice(
              seq("SEQUENTIAL", optional(seq("RELATIVE KEY IS", "data-name"))),
              seq(choice("RANDOM", "DYNAMIC"), "RELATIVE KEY IS", "data-name")
            )
          )
        ),
        optional(seq("FILE STATUS IS", "data-name"))
      ),
    input_output_section: ($) =>
      seq(
        "INPUT-OUTPUT SECTION.",
        // FILE-CONTROL is optional for MF
        optional(seq("FILE-CONTROL.", repeat($.file_control_entry))),
        optional(seq("I-O-CONTROL.", "input-output-control-entry"))
      ),
  },
});
