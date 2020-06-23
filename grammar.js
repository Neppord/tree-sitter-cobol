/* global module, optional, choice, seq, grammar, field, repeat */
const op = (...xs) => seq(...xs.map((x) => optional(x)));
const or = choice;
module.exports = grammar({
  name: "COBOL",
  rules: {
    source_file: ($) =>
      seq(
        $.identification_division,
        // Environment division is optional in ans85
        op($.environment_division)
      ),
    _user_defined_word: (_) => /[A-Za-z][A-Za-z\d_-]*/,
    program_name: ($) => $._user_defined_word,
    data_name: ($) => $._user_defined_word,
    file_name: ($) => $._user_defined_word,
    integer: (_) => /\d+/,
    literal: ($) =>
      $._user_defined_word /*or string or constant or literal number*/,
    comment_entry: (_) => /.*/,
    // identification division is optional in MF
    identification_division: ($) =>
      seq(
        "IDENTIFICATION DIVISION.", // or "ID DIVISION." in MF OSVS and VSC2
        seq(
          "PROGRAM-ID.",
          field("program_name", $.program_name),
          op(
            //  ans85
            or("IS COMMON PROGRAM", "IS INITIAL PROGRAM")
            // "IS EXTERNAL PROGRAM" MF
          )
        ),

        op(
          seq("AUTHOR.", field("author", $.comment_entry)),
          seq("INSTALLATION.", field("installation", $.comment_entry)),
          seq("DATE-WRITTEN.", field("date_written", $.comment_entry)),
          seq("DATE-COMPILED.", field("date_compiled", $.comment_entry)),
          seq("SECURITY.", field("security", $.comment_entry))
        )
        // only in OSVS where it is optional
        // + "REMARKS. comment-entry"
      ),
    environment_division: ($) =>
      seq(
        "ENVIRONMENT DIVISION.",
        op($.configuration_section),
        // Input-Output section is optional for MF
        op($.input_output_section)
      ),
    source_computer_entry: (_) => "source-computer-entry",
    object_computer_entry: (_) => "object-computer-entry",
    special_names_entry: (_) => "special-names-entry",
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
    _external_file_reference: (_) => "external-file-reference",
    _file_reference: ($) =>
      // data_name and literal both contains _user_defined_word words
      // and therefore compete
      or($._external_file_reference, /*$.data_name,*/ $.literal),
    _external_or_dynamic: (_) => or("EXTERNAL", "DYNAMIC"),
    file_control_entry: ($) =>
      or(
        $._record_sequential_file,
        $._relative_file,
        $._indexed_file,
        $._sort_merge_file
      ),
    _record_sequential_file_assign: ($) =>
      seq(
        "ASSIGN",
        op("TO"),
        or(
          seq(
            op(
              $._external_or_dynamic,
              or(
                seq(
                  or("LINE ADVANCING", seq(op("MULTIPLE"), or("REEL", "UNIT"))),
                  op("FILE")
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
        )
      ),
    _record_sequential_file_padding: ($) =>
      seq(
        "PADDING",
        op("CHARACTER", "IS"),
        // data_name and literal both contains _user_defined_word words
        // and therefore compete
        or(/*$.data_name,*/ $.literal)
      ),
    _file_status: ($) => seq(op("FILE"), "STATUS", op("IS"), $.data_name),
    _record_sequential_file: ($) =>
      seq(
        $._select,
        $._record_sequential_file_assign,
        op(
          seq("RESERVE", $.integer, $._area),
          seq(op(seq("ORGANIZATION", op("IS"))), "SEQUENTIAL"),
          $._record_sequential_file_padding,
          seq(
            "RECORD DELIMITER",
            op("IS"),
            or("STANDARD-1", "character-string")
          ),
          seq("ACCESS", op("MODE"), op("IS"), "SEQUENTIAL"),
          $._file_status
        )
      ),
    _relative_file_access_mode: ($) =>
      seq(
        "ACCESS",
        op("MODE", "IS"),
        or(
          seq(
            "SEQUENTIAL",
            op(seq("RELATIVE", op("KEY"), op("IS"), $.data_name))
          ),
          seq(or("RANDOM", "DYNAMIC"), "RELATIVE", op("KEY", "IS"), $.data_name)
        )
      ),
    _relative_file_assign: ($) =>
      seq(
        "ASSIGN",
        op("TO"),
        or(
          seq(op($._external_or_dynamic, "DISK"), $._file_reference),
          seq("DISK FROM", $.data_name),
          op($._external_or_dynamic)
        )
      ),
    _relative_file_reserve: ($) => seq("RESERVE", $.integer, $._area),
    _select: ($) => seq("SELECT", op("OPTIONAL"), $.file_name),
    _area: (_) => or("AREA", "AREAS"),
    _relative_file: ($) =>
      seq(
        $._select,
        $._relative_file_assign,
        op($._relative_file_reserve),
        seq(op("ORGANIZATION"), "RELATIVE"),
        op($._relative_file_access_mode, seq("FILE STATUS IS", $.data_name))
      ),
    _indexed_file_assign: ($) =>
      or(
        seq(op($._external_or_dynamic), op("DISK"), $._file_reference),
        seq($._external_or_dynamic),
        seq("DISK", "FROM", $._file_reference)
      ),
    _indexed_file_reserve: ($) => seq("RESERVE", $.integer, $._area),
    _indexed_file_organization: (_) => seq(or("ORGANIZATION"), "INDEXED"),
    _indexed_file_access_mode: (_) =>
      seq("ACCESS", or("MODE", "IS"), or("SEQUENTIAL", "RANDOM", "DYNAMIC")),
    _record_key: ($) => seq("RECORD", op("KEY", "IS"), $.data_name),
    _alternate_record_key: ($) =>
      seq(
        "ALTERNATE",
        op("RECORD", "KEY", "IS"),
        $.data_name,
        op(seq(op("WITH"), "DUPLICATES"))
      ),
    _indexed_file: ($) =>
      seq(
        $._select,
        op($._indexed_file_assign),
        op($._indexed_file_reserve),
        $._indexed_file_organization,
        op($._indexed_file_access_mode),
        $._record_key,
        op($._alternate_record_key),
        op($._file_status)
      ),
    _sort_merge_file: ($) =>
      seq("SELECT", $.file_name, "ASSIGN", op("TO"), $._file_reference),
    input_output_section: ($) =>
      seq(
        "INPUT-OUTPUT SECTION.",
        // FILE-CONTROL is optional for MF
        op(seq("FILE-CONTROL.", repeat($.file_control_entry))),
        op(seq("I-O-CONTROL.", "input-output-control-entry"))
      ),
  },
  conflicts: ($) => [
    [$._record_sequential_file_assign, $._relative_file_assign],
    [$._sort_merge_file, $._select],
  ],
});
