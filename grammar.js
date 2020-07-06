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
        op($.environment_division, $.data_division)
      ),
    _user_defined_word: (_) => /[A-Za-z][A-Za-z\d_-]*/,
    program_name: ($) => $._user_defined_word,
    data_name: ($) => $._user_defined_word,
    file_name: ($) => $._user_defined_word,
    implementor_name: (_) => "implementor-name",
    alphabet_name: (_) => "alphabet-name",
    integer: (_) => /\d+/,
    // TODO: implement real character string
    character_string: (_) => /"[^"]*"/,
    literal: ($) =>
      $._user_defined_word /*or string or constant or literal number*/,
    comment_entry: (_) => /.*/,
    identification_division: ($) =>
      seq(
        "IDENTIFICATION DIVISION.",
        $._identification_division_program_id,
        op(
          $._identification_division_author,
          $._identification_division_installation,
          $._identification_division_date_written,
          $._identification_division_date_compiled,
          $._identification_division_security
        )
      ),
    _identification_division_author: ($) =>
      seq("AUTHOR.", field("author", $.comment_entry)),
    _identification_division_installation: ($) =>
      seq("INSTALLATION.", field("installation", $.comment_entry)),
    _identification_division_date_written: ($) =>
      seq("DATE-WRITTEN.", field("date_written", $.comment_entry)),
    _identification_division_date_compiled: ($) =>
      seq("DATE-COMPILED.", field("date_compiled", $.comment_entry)),
    _identification_division_security: ($) =>
      seq("SECURITY.", field("security", $.comment_entry)),
    _identification_division_program_id: ($) =>
      seq(
        "PROGRAM-ID.",
        field("program_name", $.program_name),
        op($._IS, or("COMMON", "INITIAL"), "PROGRAM")
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
      seq($._ASSIGN_TO, $._record_sequential_file_assign_destination),
    _record_sequential_file_assign_destination: ($) =>
      or(
        seq(
          op($._external_or_dynamic, $._record_sequential_file_type),
          $._file_reference
        ),
        seq("DISC", "FROM", $.data_name)
      ),
    _record_sequential_file_type: ($) =>
      or(
        $._line_advancing,
        "DISC",
        "KEYBOARD",
        "DISPLAY",
        "PRINTER",
        "PRINTER-1"
      ),
    _line_advancing: (_) =>
      seq(
        or("LINE ADVANCING", seq(op("MULTIPLE"), or("REEL", "UNIT"))),
        op("FILE")
      ),
    _record_sequential_file_padding: ($) =>
      seq(
        "PADDING",
        op("CHARACTER"),
        optional($._IS),
        // data_name and literal both contains _user_defined_word words
        // and therefore compete
        or(/*$.data_name,*/ $.literal)
      ),
    _file_status: ($) =>
      seq(op("FILE"), "STATUS", optional($._IS), $.data_name),
    _record_sequential_file: ($) =>
      seq(
        $._select,
        $._record_sequential_file_assign,
        op(
          $._relative_file_reserve,
          $._record_sequential_file_organization,
          $._record_sequential_file_padding,
          $._record_sequential_file_record_delimiter,
          $._record_sequential_file_record_access_mode,
          $._file_status
        )
      ),
    _record_sequential_file_record_access_mode: ($) =>
      seq($._ACCESS_MODE_IS, "SEQUENTIAL"),
    _record_sequential_file_record_delimiter: ($) =>
      seq(
        "RECORD DELIMITER",
        optional($._IS),
        or("STANDARD-1", "character-string")
      ),
    _record_sequential_file_organization: ($) =>
      seq(op(seq("ORGANIZATION", optional($._IS))), "SEQUENTIAL"),
    _ACCESS_MODE_IS: ($) => seq("ACCESS", op("MODE"), optional($._IS)),
    _RELATIVE_KEY_IS: ($) => seq("RELATIVE", optional("KEY"), optional($._IS)),
    _relative_file_access_mode: ($) =>
      seq(
        $._ACCESS_MODE_IS,
        or(
          seq("SEQUENTIAL", op(seq($._RELATIVE_KEY_IS, $.data_name))),
          seq(or("RANDOM", "DYNAMIC"), $._RELATIVE_KEY_IS, $.data_name)
        )
      ),
    _relative_file_assign: ($) =>
      seq(
        $._ASSIGN_TO,
        or(
          seq(op($._external_or_dynamic, "DISK"), $._file_reference),
          seq("DISK FROM", $.data_name),
          op($._external_or_dynamic)
        )
      ),
    _relative_file_reserve: ($) => seq("RESERVE", $.integer, $._AREA),
    _select: ($) => seq("SELECT", op("OPTIONAL"), $.file_name),
    _AREA: (_) => or("AREA", "AREAS"),
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
    _indexed_file_reserve: ($) => seq("RESERVE", $.integer, $._AREA),
    _indexed_file_organization: (_) => seq(or("ORGANIZATION"), "INDEXED"),
    _indexed_file_access_mode: ($) =>
      seq($._ACCESS_MODE_IS, or("SEQUENTIAL", "RANDOM", "DYNAMIC")),
    _RECORD_KEY_IS: ($) => seq("RECORD", optional("KEY"), optional($._IS)),
    _WITH_DUPLICATES: (_) => seq(op("WITH"), "DUPLICATES"),
    _record_key: ($) => seq($._RECORD_KEY_IS, $.data_name),
    _alternate_record_key: ($) =>
      seq("ALTERNATE", $._RECORD_KEY_IS, $.data_name, op($._WITH_DUPLICATES)),
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
      seq("SELECT", $.file_name, $._ASSIGN_TO, $._file_reference),
    input_output_control_entry: ($) =>
      or(
        $._input_output_control_rerun_entry,
        $._input_output_control_same_entry,
        $._input_output_control_multiple_file_entry
      ),
    _input_output_control_rerun_entry: ($) =>
      seq(
        "RERUN",
        op(seq("ON", or($.file_name, $.character_string))),
        $._every
      ),
    _input_output_control_same_entry: ($) =>
      seq(
        "SAME",
        or("RECORD", "SORT", "SORT-MERGE"),
        op("AREA", "FOR"),
        repeat($.file_name)
      ),
    _input_output_control_multiple_file_entry: ($) =>
      seq(
        "MULTIPLE",
        "FILE",
        op("TAPE", "CONTAINS"),
        repeat(seq($.file_name, op(seq("POSITION", $.integer))))
      ),
    _REAL_OR_UNIT: (_) => choice("REAL", "UNIT"),
    _END_OF: (_) => seq("END", optional("OF")),
    _every: ($) =>
      seq(
        "EVERY",
        or(
          seq(op($._END_OF), $._REAL_OR_UNIT, optional("OF"), $.file_name),
          seq($.integer, "RECORDS", optional("OF"), $.file_name),
          seq($.integer, "CLOCK-UNITS"),
          /*TODO*/ "condition-name"
        )
      ),
    input_output_section: ($) =>
      seq(
        "INPUT-OUTPUT SECTION.",
        // FILE-CONTROL is optional for MF
        op(seq("FILE-CONTROL.", repeat($.file_control_entry))),
        op(seq("I-O-CONTROL.", repeat($.input_output_control_entry)))
      ),
    data_division: ($) =>
      seq(
        "DATA DIVISION.",
        op(
          $.file_section,
          $.working_storage_section,
          $.linkage_section,
          $.communication_section,
          $.report_section
        )
      ),
    file_section: ($) =>
      seq("FILE SECTION.", repeat($._file_description_entry)),
    _file_description_entry: ($) =>
      choice($._record_sequential_file_description),
    _RECORDS: ($) =>
      choice(seq("RECORD", optional($._IS)), seq("RECORDS", op("ARE"))),
    _IS: (_) => "IS",
    _ASSIGN_TO: (_) => seq("ASSIGN", optional("TO")),
    _IS_EXTERNAL: ($) => seq(optional($._IS), "EXTERNAL"),
    _IS_GLOBAL: ($) => seq(optional($._IS), "GLOBAL"),
    _LINES_AT_TOP: (_) => seq(optional("LINES"), optional("AT"), "TOP"),
    _LINES_AT_BOTTOM: (_) => seq(optional("LINES"), optional("AT"), "BOTTOM"),
    _CODE_SET_IS: ($) => seq("CODE-SET", optional($._IS)),
    _record_sequential_file_description: ($) =>
      seq(
        "FD",
        $.file_name,
        op(
          $._IS_EXTERNAL,
          $._IS_GLOBAL,
          $._record_sequential_file_block_description,
          $._record_sequential_file_record_description,
          $._record_sequential_file_label_description,
          $._record_sequential_file_value_description,
          $._record_sequential_file_data_description,
          $._record_sequential_file_linage_description,
          seq($._LINES_AT_TOP, $.data_name),
          seq($._LINES_AT_BOTTOM, $.data_name),
          seq($._CODE_SET_IS, $.alphabet_name)
        )
      ),
    _record_sequential_file_block_description: ($) =>
      seq(
        "BLOCK",
        op("CONTAINS"),
        op(seq($.integer, "TO")),
        $.integer,
        choice("RECORDS", "CHARACTERS")
      ),
    _record_sequential_file_record_description: ($) =>
      seq("RECORD", choice($._ansi85_contains, $._std_contains)),
    _ansi85_contains: ($) =>
      seq(
        op("CONTAINS"),
        $.integer,
        op("CHARACTERS"),
        optional($._IS),
        "VARYING",
        op("IN", "SIZE"),
        op(
          seq(
            op(seq(op("FROM"), $.integer), seq("TO", $.integer), "CHARACTERS"),
            op(seq("DEPENDING", op("ON"), $.data_name))
          )
        )
      ),
    _std_contains: ($) =>
      seq(
        op("CONTAINS"),
        op(seq($.integer, "TO")),
        $.integer,
        op("CHARACTERS")
      ),
    _record_sequential_file_label_description: ($) =>
      seq("LABEL", $._RECORDS, choice("STANDARD", "OMITTED")),
    _record_sequential_file_value_description: ($) =>
      seq(
        "VALUE",
        "OF",
        $.implementor_name,
        optional($._IS),
        repeat($.data_name)
      ),
    _record_sequential_file_data_description: ($) =>
      seq("DATA", $._RECORDS, repeat($.data_name)),
    _record_sequential_file_linage_description: ($) =>
      seq(
        "LINAGE",
        optional($._IS),
        $.data_name,
        op("LINES"),
        op(seq(op("WITH"), "FOOTING", op("AT"), $.data_name))
      ),
    working_storage_section: (_) => "WORKING-STORAGE SECTION.",
    linkage_section: (_) => "LINKAGE SECTION.",
    communication_section: (_) => "COMMUNICATION SECTION.",
    report_section: (_) => "REPORT SECTION.",
  },
  conflicts: ($) => [
    [$._record_sequential_file_assign, $._relative_file_assign],
    [$._sort_merge_file, $._select],
    [$._record_sequential_file_linage_description],
    [$._record_sequential_file_assign_destination, $._relative_file_assign],
  ],
});
