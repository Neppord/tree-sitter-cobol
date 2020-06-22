module.exports = grammar({
  name: "COBOL",
  rules: {
    source_file: ($) =>
      seq(
        $.identification_division,
        // Environment division is optional in ans85
        optional($.environment_division)
      ),
    _user_defined_word: ($) => /[A-Za-z][A-Za-z\d_-]*/,
    program_name: ($) => $._user_defined_word,
    data_name: ($) => $._user_defined_word,
    file_name: ($) => $._user_defined_word,
    integer: ($) => /\d+/,
    literal: ($) =>
      $._user_defined_word /*or string or constant or literal number*/,
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
    // _external_file_reference is a string
    _external_file_reference: ($) => "external-file-reference",
    _file_reference: ($) =>
      // data_name and literal both contains _user_defined_word words
      // and therefore compete
      choice($._external_file_reference, /*$.data_name,*/ $.literal),
    _external_or_dynamic: ($) => choice("EXTERNAL", "DYNAMIC"),
    file_control_entry: ($) =>
      choice($._record_sequential_file, $._relative_file),
    _foo: ($) =>
      choice(
        seq(
          optional($._external_or_dynamic),
          optional(
            choice(
              seq(
                choice(
                  "LINE ADVANCING",
                  seq(optional("MULTIPLE"), choice("REEL", "UNIT"))
                ),
                optional("FILE")
              ),
              "DISC",
              "KEYBOARD",
              "DISPLAY",
              "PRINTER",
              "PRINTER-1"
            )
          ),
          $._file_reference
        ),
        seq("DISC", "FROM", $.data_name)
      ),
    _record_sequential_file: ($) =>
      seq(
        "SELECT",
        optional("OPTIONAL"),
        $.file_name,
        "ASSIGN",
        optional("TO"),
        $._foo,
        optional(seq("RESERVE", $.integer, choice("AREA", "AREAS"))),
        optional(
          seq(optional(seq("ORGANIZATION", optional("IS"))), "SEQUENTIAL")
        ),
        optional(
          seq(
            "PADDING",
            optional("CHARACTER"),
            optional("IS"),
            // data_name and literal both contains _user_defined_word words
            // and therefore compete
            choice(/*$.data_name,*/ $.literal)
          )
        ),
        optional(
          seq(
            "RECORD DELIMITER",
            optional("IS"),
            choice("STANDARD-1", "character-string")
          )
        ),
        optional(seq("ACCESS", optional("MODE"), optional("IS"), "SEQUENTIAL")),
        optional(seq("FILE STATUS IS", $.data_name))
      ),
    _relative_file: ($) =>
      seq(
        "SELECT",
        optional("OPTIONAL"),
        $.file_name,
        "ASSIGN",
        optional("TO"),
        choice(
          seq(
            optional($._external_or_dynamic),
            optional("DISK"),
            $._file_reference
          ),
          seq("DISK FROM", $.data_name),
          optional($._external_or_dynamic)
        ),
        optional(seq("RESERVE", $.integer, choice("AREA", "AREAS"))),
        seq(optional("ORGANIZATION"), "RELATIVE"),
        optional(
          seq(
            "ACCESS",
            optional("MODE"),
            optional("IS"),
            choice(
              seq(
                "SEQUENTIAL",
                optional(
                  seq("RELATIVE", optional("KEY"), optional("IS"), $.data_name)
                )
              ),
              seq(
                choice("RANDOM", "DYNAMIC"),
                "RELATIVE",
                optional("KEY"),
                optional("IS"),
                $.data_name
              )
            )
          )
        ),
        optional(seq("FILE STATUS IS", $.data_name))
      ),
    input_output_section: ($) =>
      seq(
        "INPUT-OUTPUT SECTION.",
        // FILE-CONTROL is optional for MF
        optional(seq("FILE-CONTROL.", repeat($.file_control_entry))),
        optional(seq("I-O-CONTROL.", "input-output-control-entry"))
      ),
  },
  conflicts: ($) => [[$._foo, $._relative_file]],
});
