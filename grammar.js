/* global module, optional, choice, seq, grammar, field, repeat, repeat1 */
module.exports = grammar({
  name: "Cobol",
  rules: {
    compilationUnit: ($) => repeat1($.programUnit),
    programUnit: ($) =>
      seq(
        $.identificationDivision,
        optional($.environmentDivision),
        optional($.dataDivision),
        optional($.procedureDivision),
        repeat($.programUnit),
        optional($.endProgramStatement)
      ),
    endProgramStatement: ($) => seq($.END, $.PROGRAM, $.programName, $.DOT_FS),
    // --- identification division --------------------------------------------------------------------
    identificationDivision: ($) =>
      seq(
        choice($.IDENTIFICATION, $.ID),
        $.DIVISION,
        $.DOT_FS,
        $.programIdParagraph,
        repeat($._identificationDivisionBody)
      ),
    _identificationDivisionBody: ($) =>
      choice(
        $.authorParagraph,
        $.installationParagraph,
        $.dateWrittenParagraph,
        $.dateCompiledParagraph,
        $.securityParagraph,
        $.remarksParagraph
      ),
    // - program id paragraph ----------------------------------
    programIdParagraph: ($) =>
      seq(
        $.PROGRAM_ID,
        $.DOT_FS,
        $.programName,
        optional(
          seq(
            optional($.IS),
            choice($.COMMON, $.INITIAL, $.LIBRARY, $.DEFINITION, $.RECURSIVE),
            optional($.PROGRAM)
          )
        ),
        optional($.DOT_FS)
      ),
    // - author paragraph ----------------------------------
    authorParagraph: ($) => seq($.AUTHOR, $.DOT_FS, optional($.commentEntry)),
    // - installation paragraph ----------------------------------
    installationParagraph: ($) =>
      seq($.INSTALLATION, $.DOT_FS, optional($.commentEntry)),
    // - date written paragraph ----------------------------------
    dateWrittenParagraph: ($) =>
      seq($.DATE_WRITTEN, $.DOT_FS, optional($.commentEntry)),
    // - date compiled paragraph ----------------------------------
    dateCompiledParagraph: ($) =>
      seq($.DATE_COMPILED, $.DOT_FS, optional($.commentEntry)),
    // - security paragraph ----------------------------------
    securityParagraph: ($) =>
      seq($.SECURITY, $.DOT_FS, optional($.commentEntry)),
    // - remarks paragraph ----------------------------------
    remarksParagraph: ($) =>
      seq(
        $.REMARKS,
        $.DOT_FS,
        optional($.commentEntry),
        optional($.END_REMARKS),
        optional($.DOT_FS)
      ),
    // --- environment division --------------------------------------------------------------------
    environmentDivision: ($) =>
      seq(
        $.ENVIRONMENT,
        $.DIVISION,
        $.DOT_FS,
        repeat($.environmentDivisionBody)
      ),
    environmentDivisionBody: ($) =>
      choice(
        $.configurationSection,
        $.specialNamesParagraph,
        $.inputOutputSection
      ),
    // -- configuration section ----------------------------------
    configurationSection: ($) =>
      seq(
        $.CONFIGURATION,
        $.SECTION,
        $.DOT_FS,
        repeat($.configurationSectionParagraph)
      ),
    // - configuration section paragraph ----------------------------------
    configurationSectionParagraph: ($) =>
      choice(
        $.sourceComputerParagraph,
        $.objectComputerParagraph,
        $.specialNamesParagraph
      ),
    // $.strictly, specialNamesParagraph does not belong into $.configurationSectionParagraph, but IBM-COBOL allows $.this,
    // - source computer paragraph ----------------------------------
    sourceComputerParagraph: ($) =>
      seq(
        $.SOURCE_COMPUTER,
        $.DOT_FS,
        optional(
          seq(
            $.computerName,
            optional(seq(optional($.WITH), $.DEBUGGING, $.MODE)),
            $.DOT_FS
          )
        )
      ),
    // - object computer paragraph ----------------------------------
    objectComputerParagraph: ($) =>
      seq(
        $.OBJECT_COMPUTER,
        $.DOT_FS,
        optional(seq($.computerName, repeat($.objectComputerClause), $.DOT_FS))
      ),
    objectComputerClause: ($) =>
      choice(
        $.memorySizeClause,
        $.diskSizeClause,
        $.collatingSequenceClause,
        $.segmentLimitClause,
        $.characterSetClause
      ),
    memorySizeClause: ($) =>
      seq(
        $.MEMORY,
        optional($.SIZE),
        choice($.integerLiteral, $.cobolWord),
        optional(choice($.WORDS, $.CHARACTERS, $.MODULES))
      ),
    diskSizeClause: ($) =>
      seq(
        $.DISK,
        optional($.SIZE),
        optional($.IS),
        choice($.integerLiteral, $.cobolWord),
        optional(choice($.WORDS, $.MODULES))
      ),
    collatingSequenceClause: ($) =>
      seq(
        optional($.PROGRAM),
        optional($.COLLATING),
        $.SEQUENCE,
        seq(optional($.IS), repeat1($.alphabetName)),
        optional($.collatingSequenceClauseAlphanumeric),
        optional($.collatingSequenceClauseNational)
      ),
    collatingSequenceClauseAlphanumeric: ($) =>
      seq(optional($.FOR), $.ALPHANUMERIC, optional($.IS), $.alphabetName),
    collatingSequenceClauseNational: ($) =>
      prec(2, seq(optional($.FOR), $.NATIONAL, optional($.IS), $.alphabetName)),
    segmentLimitClause: ($) =>
      seq($.SEGMENT_LIMIT, optional($.IS), $.integerLiteral),
    characterSetClause: ($) => seq($.CHARACTER, $.SET, $.DOT_FS),
    // - special names paragraph ----------------------------------
    specialNamesParagraph: ($) =>
      seq(
        $.SPECIAL_NAMES,
        $.DOT_FS,
        optional(seq(repeat1($.specialNameClause), $.DOT_FS))
      ),
    specialNameClause: ($) =>
      choice(
        $.channelClause,
        $.odtClause,
        $.alphabetClause,
        $.classClause,
        $.currencySignClause,
        $.decimalPointClause,
        $.symbolicCharactersClause,
        $.environmentSwitchNameClause,
        $.defaultDisplaySignClause,
        $.defaultComputationalSignClause,
        $.reserveNetworkClause
      ),
    alphabetClause: ($) =>
      choice($.alphabetClauseFormat1, $.alphabetClauseFormat2),
    alphabetClauseFormat1: ($) =>
      seq(
        $.ALPHABET,
        $.alphabetName,
        optional(seq($.FOR, $.ALPHANUMERIC)),
        optional($.IS),
        seq(
          choice(
            $.EBCDIC,
            $.ASCII,
            $.STANDARD_1,
            $.STANDARD_2,
            $.NATIVE,
            $.cobolWord,
            repeat1($.alphabetLiterals)
          )
        )
      ),
    alphabetLiterals: ($) =>
      seq(
        $.literal,
        optional(choice($.alphabetThrough, repeat1($.alphabetAlso)))
      ),
    alphabetThrough: ($) => seq(choice($.THROUGH, $.THRU), $.literal),
    alphabetAlso: ($) => seq($.ALSO, repeat1($.literal)),
    alphabetClauseFormat2: ($) =>
      seq(
        $.ALPHABET,
        $.alphabetName,
        optional($.FOR),
        $.NATIONAL,
        optional($.IS),
        seq(choice($.NATIVE, $.CCSVERSION), $.literal)
      ),
    channelClause: ($) =>
      seq($.CHANNEL, $.integerLiteral, optional($.IS), $.mnemonicName),
    classClause: ($) =>
      seq(
        $.CLASS,
        $.className,
        optional(seq(optional($.FOR), choice($.ALPHANUMERIC, $.NATIONAL))),
        optional($.IS),
        repeat1($.classClauseThrough)
      ),
    classClauseThrough: ($) =>
      seq(
        $.classClauseFrom,
        optional(seq(choice($.THROUGH, $.THRU), $.classClauseTo))
      ),
    classClauseFrom: ($) => choice($.identifier, $.literal),
    classClauseTo: ($) => choice($.identifier, $.literal),
    currencySignClause: ($) =>
      seq(
        $.CURRENCY,
        optional($.SIGN),
        optional($.IS),
        $.literal,
        optional(seq(optional($.WITH), $.PICTURE, $.SYMBOL, $.literal))
      ),
    decimalPointClause: ($) => seq($.DECIMAL_POINT, optional($.IS), $.COMMA),
    defaultComputationalSignClause: ($) =>
      prec(
        2,
        seq(
          $.DEFAULT,
          optional(choice($.COMPUTATIONAL, $.COMP)),
          optional(seq($.SIGN, optional($.IS))),
          optional(choice($.LEADING, $.TRAILING)),
          seq($.SEPARATE, optional($.CHARACTER))
        )
      ),
    defaultDisplaySignClause: ($) =>
      prec(
        2,
        seq(
          $.DEFAULT_DISPLAY,
          optional(seq($.SIGN, optional($.IS))),
          choice($.LEADING, $.TRAILING),
          optional(seq($.SEPARATE, optional($.CHARACTER)))
        )
      ),
    environmentSwitchNameClause: ($) =>
      seq(
        $.environmentName,
        optional($.IS),
        $.mnemonicName,
        choice(
          optional($.environmentSwitchNameSpecialNamesStatusPhrase),
          $.environmentSwitchNameSpecialNamesStatusPhrase
        )
      ),
    environmentSwitchNameSpecialNamesStatusPhrase: ($) =>
      seq(
        $.ON,
        optional($.STATUS),
        optional($.IS),
        $.condition,
        choice(
          optional(seq($.OFF, optional($.STATUS), optional($.IS), $.condition)),
          $.OFF
        ),
        optional($.STATUS),
        optional($.IS),
        $.condition,
        optional(seq($.ON, optional($.STATUS), optional($.IS), $.condition))
      ),
    odtClause: ($) => prec(2, seq($.ODT, optional($.IS), $.mnemonicName)),
    reserveNetworkClause: ($) =>
      seq(
        $.RESERVE,
        optional($.WORDS),
        optional($.LIST),
        optional($.IS),
        $.NETWORK,
        optional($.CAPABLE)
      ),
    symbolicCharactersClause: ($) =>
      seq(
        $.SYMBOLIC,
        optional($.CHARACTERS),
        optional(seq(optional($.FOR), choice($.ALPHANUMERIC, $.NATIONAL))),
        repeat1($.symbolicCharacters),
        optional(seq($.IN, $.alphabetName))
      ),
    symbolicCharacters: ($) =>
      seq(
        repeat1($.symbolicCharacter),
        optional(choice($.IS, $.ARE)),
        repeat1($.integerLiteral)
      ),
    // -- input output section ----------------------------------
    inputOutputSection: ($) =>
      seq(
        $.INPUT_OUTPUT,
        $.SECTION,
        $.DOT_FS,
        repeat($.inputOutputSectionParagraph)
      ),
    // - input output section paragraph ----------------------------------
    inputOutputSectionParagraph: ($) =>
      choice($.fileControlParagraph, $.ioControlParagraph),
    // - file control paragraph ----------------------------------
    fileControlParagraph: ($) =>
      seq(
        optional($.FILE_CONTROL),
        repeat(seq(optional($.DOT_FS), $.fileControlEntry)),
        $.DOT_FS
      ),
    fileControlEntry: ($) => seq($.selectClause, repeat($.fileControlClause)),
    selectClause: ($) => seq($.SELECT, optional($.OPTIONAL), $.fileName),
    fileControlClause: ($) =>
      choice(
        $.assignClause,
        $.reserveClause,
        $.organizationClause,
        $.paddingCharacterClause,
        $.recordDelimiterClause,
        $.accessModeClause,
        $.recordKeyClause,
        $.alternateRecordKeyClause,
        $.fileStatusClause,
        $.passwordClause,
        $.relativeKeyClause
      ),
    assignClause: ($) =>
      seq(
        $.ASSIGN,
        optional($.TO),
        choice(
          $.DISK,
          $.DISPLAY,
          $.KEYBOARD,
          $.PORT,
          $.PRINTER,
          $.READER,
          $.REMOTE,
          $.TAPE,
          $.VIRTUAL,
          seq(optional(choice($.DYNAMIC, $.EXTERNAL)), $.assignmentName),
          $.literal
        )
      ),
    reserveClause: ($) =>
      seq(
        $.RESERVE,
        choice($.NO, $.integerLiteral),
        optional($.ALTERNATE),
        optional(choice($.AREA, $.AREAS))
      ),
    organizationClause: ($) =>
      seq(
        optional(seq($.ORGANIZATION, optional($.IS))),
        optional(
          seq(choice($.LINE, $.RECORD), choice($.BINARY, $.RECORD, $.BINARY))
        ),
        choice($.SEQUENTIAL, $.RELATIVE, $.INDEXED)
      ),
    paddingCharacterClause: ($) =>
      seq(
        $.PADDING,
        optional($.CHARACTER),
        optional($.IS),
        choice($.qualifiedDataName, $.literal)
      ),
    recordDelimiterClause: ($) =>
      seq(
        $.RECORD,
        $.DELIMITER,
        optional($.IS),
        choice($.STANDARD_1, $.IMPLICIT, $.assignmentName)
      ),
    accessModeClause: ($) =>
      seq(
        $.ACCESS,
        optional($.MODE),
        optional($.IS),
        choice($.SEQUENTIAL, $.RANDOM, $.DYNAMIC, $.EXCLUSIVE)
      ),
    recordKeyClause: ($) =>
      seq(
        $.RECORD,
        optional($.KEY),
        optional($.IS),
        $.qualifiedDataName,
        optional($.passwordClause),
        optional(seq(optional($.WITH), $.DUPLICATES))
      ),
    alternateRecordKeyClause: ($) =>
      seq(
        $.ALTERNATE,
        $.RECORD,
        optional($.KEY),
        optional($.IS),
        $.qualifiedDataName,
        optional($.passwordClause),
        optional(seq(optional($.WITH), $.DUPLICATES))
      ),
    passwordClause: ($) => seq($.PASSWORD, optional($.IS), $.dataName),
    fileStatusClause: ($) =>
      seq(
        optional($.FILE),
        $.STATUS,
        optional($.IS),
        $.qualifiedDataName,
        optional($.qualifiedDataName)
      ),
    relativeKeyClause: ($) =>
      seq($.RELATIVE, optional($.KEY), optional($.IS), $.qualifiedDataName),
    // - io control paragraph ----------------------------------
    ioControlParagraph: ($) =>
      seq(
        $.I_O_CONTROL,
        $.DOT_FS,
        optional(seq($.fileName, $.DOT_FS)),
        optional(seq(repeat($.ioControlClause), $.DOT_FS))
      ),
    ioControlClause: ($) =>
      choice(
        $.rerunClause,
        $.sameClause,
        $.multipleFileClause,
        $.commitmentControlClause
      ),
    rerunClause: ($) =>
      seq(
        $.RERUN,
        optional(seq($.ON, choice($.assignmentName, $.fileName))),
        $.EVERY,
        choice($.rerunEveryRecords, $.rerunEveryOf, $.rerunEveryClock)
      ),
    rerunEveryRecords: ($) => seq($.integerLiteral, $.RECORDS),
    rerunEveryOf: ($) =>
      seq(
        optional($.END),
        optional($.OF),
        choice($.REEL, $.UNIT),
        $.OF,
        $.fileName
      ),
    rerunEveryClock: ($) => seq($.integerLiteral, optional($.CLOCK_UNITS)),
    sameClause: ($) =>
      seq(
        $.SAME,
        optional(choice($.RECORD, $.SORT, $.SORT_MERGE)),
        optional($.AREA),
        optional($.FOR),
        repeat1($.fileName)
      ),
    multipleFileClause: ($) =>
      seq(
        $.MULTIPLE,
        $.FILE,
        optional($.TAPE),
        optional($.CONTAINS),
        repeat1($.multipleFilePosition)
      ),
    multipleFilePosition: ($) =>
      seq($.fileName, optional(seq($.POSITION, $.integerLiteral))),
    commitmentControlClause: ($) =>
      seq($.COMMITMENT, $.CONTROL, optional($.FOR), $.fileName),
    // --- data division --------------------------------------------------------------------
    dataDivision: ($) =>
      seq($.DATA, $.DIVISION, $.DOT_FS, repeat($.dataDivisionSection)),
    dataDivisionSection: ($) =>
      choice(
        $.fileSection,
        $.dataBaseSection,
        $.workingStorageSection,
        $.linkageSection,
        $.communicationSection,
        $.localStorageSection,
        $.screenSection,
        $.reportSection,
        $.programLibrarySection
      ),
    // -- file section ----------------------------------
    fileSection: ($) =>
      seq($.FILE, $.SECTION, $.DOT_FS, repeat($.fileDescriptionEntry)),
    fileDescriptionEntry: ($) =>
      seq(
        choice($.FD, $.SD),
        $.fileName,
        repeat(seq(optional($.DOT_FS), $.fileDescriptionEntryClause)),
        $.DOT_FS,
        repeat($.dataDescriptionEntry)
      ),
    fileDescriptionEntryClause: ($) =>
      choice(
        $.externalClause,
        $.globalClause,
        $.blockContainsClause,
        $.recordContainsClause,
        $.labelRecordsClause,
        $.valueOfClause,
        $.dataRecordsClause,
        $.linageClause,
        $.codeSetClause,
        $.reportClause,
        $.recordingModeClause
      ),
    externalClause: ($) => seq(optional($.IS), $.EXTERNAL),
    globalClause: ($) => seq(optional($.IS), $.GLOBAL),
    blockContainsClause: ($) =>
      seq(
        $.BLOCK,
        optional($.CONTAINS),
        $.integerLiteral,
        optional($.blockContainsTo),
        optional(choice($.RECORDS, $.CHARACTERS))
      ),
    blockContainsTo: ($) => seq($.TO, $.integerLiteral),
    recordContainsClause: ($) =>
      seq(
        $.RECORD,
        choice(
          $.recordContainsClauseFormat1,
          $.recordContainsClauseFormat2,
          $.recordContainsClauseFormat3
        )
      ),
    recordContainsClauseFormat1: ($) =>
      seq(optional($.CONTAINS), $.integerLiteral, optional($.CHARACTERS)),
    recordContainsClauseFormat2: ($) =>
      seq(
        optional($.IS),
        $.VARYING,
        optional($.IN),
        optional($.SIZE),
        optional(
          seq(
            optional($.FROM),
            $.integerLiteral,
            optional($.recordContainsTo),
            optional($.CHARACTERS)
          )
        ),
        optional(seq($.DEPENDING, optional($.ON), $.qualifiedDataName))
      ),
    recordContainsClauseFormat3: ($) =>
      seq(
        optional($.CONTAINS),
        $.integerLiteral,
        $.recordContainsTo,
        optional($.CHARACTERS)
      ),
    recordContainsTo: ($) => seq($.TO, $.integerLiteral),
    labelRecordsClause: ($) =>
      seq(
        $.LABEL,
        choice(seq($.RECORD, optional($.IS)), seq($.RECORDS, optional($.ARE))),
        choice($.OMITTED, $.STANDARD, repeat1($.dataName))
      ),
    valueOfClause: ($) => seq($.VALUE, $.OF, repeat1($.valuePair)),
    valuePair: ($) =>
      seq($.systemName, optional($.IS), choice($.qualifiedDataName, $.literal)),
    dataRecordsClause: ($) =>
      seq(
        $.DATA,
        choice(seq($.RECORD, optional($.IS)), seq($.RECORDS, optional($.ARE))),
        repeat1($.dataName)
      ),
    linageClause: ($) =>
      seq(
        $.LINAGE,
        optional($.IS),
        choice($.dataName, $.integerLiteral),
        optional($.LINES),
        repeat($.linageAt)
      ),
    linageAt: ($) =>
      choice($.linageFootingAt, $.linageLinesAtTop, $.linageLinesAtBottom),
    linageFootingAt: ($) =>
      seq(
        optional($.WITH),
        $.FOOTING,
        optional($.AT),
        choice($.dataName, $.integerLiteral)
      ),
    linageLinesAtTop: ($) =>
      seq(
        optional($.LINES),
        optional($.AT),
        $.TOP,
        choice($.dataName, $.integerLiteral)
      ),
    linageLinesAtBottom: ($) =>
      seq(
        optional($.LINES),
        optional($.AT),
        $.BOTTOM,
        choice($.dataName, $.integerLiteral)
      ),
    recordingModeClause: ($) =>
      seq($.RECORDING, optional($.MODE), optional($.IS), $.modeStatement),
    modeStatement: ($) => $.cobolWord,
    codeSetClause: ($) => seq($.CODE_SET, optional($.IS), $.alphabetName),
    reportClause: ($) =>
      seq(
        choice(seq($.REPORT, optional($.IS)), seq($.REPORTS, optional($.ARE))),
        repeat1($.reportName)
      ),
    // -- data base section ----------------------------------
    dataBaseSection: ($) =>
      seq($.DATA_BASE, $.SECTION, $.DOT_FS, repeat($.dataBaseSectionEntry)),
    dataBaseSectionEntry: ($) =>
      seq($.integerLiteral, $.literal, $.INVOKE, $.literal),
    // -- working storage section ----------------------------------
    workingStorageSection: ($) =>
      seq(
        $.WORKING_STORAGE,
        $.SECTION,
        $.DOT_FS,
        repeat($.dataDescriptionEntry)
      ),
    // -- linkage section ----------------------------------
    linkageSection: ($) =>
      seq($.LINKAGE, $.SECTION, $.DOT_FS, repeat($.dataDescriptionEntry)),
    // -- communication section ----------------------------------
    communicationSection: ($) =>
      seq(
        $.COMMUNICATION,
        $.SECTION,
        $.DOT_FS,
        repeat(choice($.communicationDescriptionEntry, $.dataDescriptionEntry))
      ),
    communicationDescriptionEntry: ($) =>
      choice(
        $.communicationDescriptionEntryFormat1,
        $.communicationDescriptionEntryFormat2,
        $.communicationDescriptionEntryFormat3
      ),
    communicationDescriptionEntryFormat1: ($) =>
      seq(
        $.CD,
        $.cdName,
        optional($.FOR),
        optional($.INITIAL),
        $.INPUT,
        repeat(
          choice(
            $.symbolicQueueClause,
            $.symbolicSubQueueClause,
            $.messageDateClause,
            $.messageTimeClause,
            $.symbolicSourceClause,
            $.textLengthClause,
            $.endKeyClause,
            $.statusKeyClause,
            $.messageCountClause,
            $.dataDescName
          )
        ),
        $.DOT_FS
      ),
    communicationDescriptionEntryFormat2: ($) =>
      seq(
        $.CD,
        $.cdName,
        optional($.FOR),
        $.OUTPUT,
        repeat(
          choice(
            $.destinationCountClause,
            $.textLengthClause,
            $.statusKeyClause,
            $.destinationTableClause,
            $.errorKeyClause,
            $.symbolicDestinationClause
          )
        ),
        $.DOT_FS
      ),
    communicationDescriptionEntryFormat3: ($) =>
      seq(
        $.CD,
        $.cdName,
        optional($.FOR),
        $.INITIAL,
        $.I_O,
        repeat(
          choice(
            $.messageDateClause,
            $.messageTimeClause,
            $.symbolicTerminalClause,
            $.textLengthClause,
            $.endKeyClause,
            $.statusKeyClause,
            $.dataDescName
          )
        ),
        $.DOT_FS
      ),
    destinationCountClause: ($) =>
      seq($.DESTINATION, $.COUNT, optional($.IS), $.dataDescName),
    destinationTableClause: ($) =>
      seq(
        $.DESTINATION,
        $.TABLE,
        $.OCCURS,
        $.integerLiteral,
        $.TIMES,
        optional(seq($.INDEXED, $.BY, repeat1($.indexName)))
      ),
    endKeyClause: ($) => seq($.END, $.KEY, optional($.IS), $.dataDescName),
    errorKeyClause: ($) => seq($.ERROR, $.KEY, optional($.IS), $.dataDescName),
    messageCountClause: ($) =>
      seq(optional($.MESSAGE), $.COUNT, optional($.IS), $.dataDescName),
    messageDateClause: ($) =>
      seq($.MESSAGE, $.DATE, optional($.IS), $.dataDescName),
    messageTimeClause: ($) =>
      seq($.MESSAGE, $.TIME, optional($.IS), $.dataDescName),
    statusKeyClause: ($) =>
      seq($.STATUS, $.KEY, optional($.IS), $.dataDescName),
    symbolicDestinationClause: ($) =>
      seq(optional($.SYMBOLIC), $.DESTINATION, optional($.IS), $.dataDescName),
    symbolicQueueClause: ($) =>
      seq(optional($.SYMBOLIC), $.QUEUE, optional($.IS), $.dataDescName),
    symbolicSourceClause: ($) =>
      seq(optional($.SYMBOLIC), $.SOURCE, optional($.IS), $.dataDescName),
    symbolicTerminalClause: ($) =>
      seq(optional($.SYMBOLIC), $.TERMINAL, optional($.IS), $.dataDescName),
    symbolicSubQueueClause: ($) =>
      seq(
        optional($.SYMBOLIC),
        choice($.SUB_QUEUE_1, $.SUB_QUEUE_2, $.SUB_QUEUE_3),
        optional($.IS),
        $.dataDescName
      ),
    textLengthClause: ($) =>
      seq($.TEXT, $.LENGTH, optional($.IS), $.dataDescName),
    // -- local storage section ----------------------------------
    localStorageSection: ($) =>
      seq(
        $.LOCAL_STORAGE,
        $.SECTION,
        $.DOT_FS,
        optional(seq($.LD, $.localName, $.DOT_FS)),
        repeat($.dataDescriptionEntry)
      ),
    // -- screen section ----------------------------------
    screenSection: ($) =>
      seq($.SCREEN, $.SECTION, $.DOT_FS, repeat($.screenDescriptionEntry)),
    screenDescriptionEntry: ($) =>
      seq(
        $.INTEGERLITERAL,
        optional(choice($.FILLER, $.screenName)),
        repeat(
          choice(
            $.screenDescriptionBlankClause,
            $.screenDescriptionBellClause,
            $.screenDescriptionBlinkClause,
            $.screenDescriptionEraseClause,
            $.screenDescriptionLightClause,
            $.screenDescriptionGridClause,
            $.screenDescriptionReverseVideoClause,
            $.screenDescriptionUnderlineClause,
            $.screenDescriptionSizeClause,
            $.screenDescriptionLineClause,
            $.screenDescriptionColumnClause,
            $.screenDescriptionForegroundColorClause,
            $.screenDescriptionBackgroundColorClause,
            $.screenDescriptionControlClause,
            $.screenDescriptionValueClause,
            $.screenDescriptionPictureClause,
            $.screenDescriptionFromClause,
            $.screenDescriptionUsingClause,
            $.screenDescriptionUsageClause,
            $.screenDescriptionBlankWhenZeroClause,
            $.screenDescriptionJustifiedClause,
            $.screenDescriptionSignClause,
            $.screenDescriptionAutoClause,
            $.screenDescriptionSecureClause,
            $.screenDescriptionRequiredClause,
            $.screenDescriptionPromptClause,
            $.screenDescriptionFullClause,
            $.screenDescriptionZeroFillClause
          )
        ),
        $.DOT_FS
      ),
    screenDescriptionBlankClause: ($) => seq($.BLANK, choice($.SCREEN, $.LINE)),
    screenDescriptionBellClause: ($) => choice($.BELL, $.BEEP),
    screenDescriptionBlinkClause: ($) => $.BLINK,
    screenDescriptionEraseClause: ($) => seq($.ERASE, choice($.EOL, $.EOS)),
    screenDescriptionLightClause: ($) => choice($.HIGHLIGHT, $.LOWLIGHT),
    screenDescriptionGridClause: ($) => choice($.GRID, $.LEFTLINE, $.OVERLINE),
    screenDescriptionReverseVideoClause: ($) => $.REVERSE_VIDEO,
    screenDescriptionUnderlineClause: ($) => $.UNDERLINE,
    screenDescriptionSizeClause: ($) =>
      seq($.SIZE, optional($.IS), choice($.identifier, $.integerLiteral)),
    screenDescriptionLineClause: ($) =>
      seq(
        $.LINE,
        optional(
          seq(
            optional($.NUMBER),
            optional($.IS),
            choice($.PLUS, $.PLUSCHAR, $._MINUSCHAR)
          )
        ),
        choice($.identifier, $.integerLiteral)
      ),
    screenDescriptionColumnClause: ($) =>
      seq(
        choice($.COLUMN, $.COL),
        optional(
          seq(
            optional($.NUMBER),
            optional($.IS),
            choice($.PLUS, $.PLUSCHAR, $._MINUSCHAR)
          )
        ),
        choice($.identifier, $.integerLiteral)
      ),
    screenDescriptionForegroundColorClause: ($) =>
      seq(
        choice($.FOREGROUND_COLOR, $.FOREGROUND_COLOUR),
        optional($.IS),
        choice($.identifier, $.integerLiteral)
      ),
    screenDescriptionBackgroundColorClause: ($) =>
      seq(
        choice($.BACKGROUND_COLOR, $.BACKGROUND_COLOUR),
        optional($.IS),
        choice($.identifier, $.integerLiteral)
      ),
    screenDescriptionControlClause: ($) =>
      seq($.CONTROL, optional($.IS), $.identifier),
    screenDescriptionValueClause: ($) =>
      seq($.VALUE, optional($.IS), $.literal),
    screenDescriptionPictureClause: ($) =>
      seq(choice($.PICTURE, $.PIC), optional($.IS), $.pictureString),
    screenDescriptionFromClause: ($) =>
      seq(
        $.FROM,
        choice($.identifier, $.literal),
        optional($.screenDescriptionToClause)
      ),
    screenDescriptionToClause: ($) => seq($.TO, $.identifier),
    screenDescriptionUsingClause: ($) => seq($.USING, $.identifier),
    screenDescriptionUsageClause: ($) =>
      seq($.USAGE, optional($.IS), choice($.DISPLAY, $.DISPLAY_1)),
    screenDescriptionBlankWhenZeroClause: ($) =>
      seq($.BLANK, optional($.WHEN), $.ZERO),
    screenDescriptionJustifiedClause: ($) =>
      seq(choice($.JUSTIFIED, $.JUST), optional($.RIGHT)),
    screenDescriptionSignClause: ($) =>
      seq(
        optional(seq($.SIGN, optional($.IS))),
        choice($.LEADING, $.TRAILING),
        optional(seq($.SEPARATE, optional($.CHARACTER)))
      ),
    screenDescriptionAutoClause: ($) => choice($.AUTO, $.AUTO_SKIP),
    screenDescriptionSecureClause: ($) => choice($.SECURE, $.NO_ECHO),
    screenDescriptionRequiredClause: ($) => choice($.REQUIRED, $.EMPTY_CHECK),
    screenDescriptionPromptClause: ($) =>
      seq(
        $.PROMPT,
        optional($.CHARACTER),
        optional($.IS),
        choice($.identifier, $.literal),
        optional($.screenDescriptionPromptOccursClause)
      ),
    screenDescriptionPromptOccursClause: ($) =>
      seq($.OCCURS, $.integerLiteral, optional($.TIMES)),
    screenDescriptionFullClause: ($) => choice($.FULL, $.LENGTH_CHECK),
    screenDescriptionZeroFillClause: ($) => $.ZERO_FILL,
    // -- report section ----------------------------------
    reportSection: ($) =>
      seq($.REPORT, $.SECTION, $.DOT_FS, repeat($.reportDescription)),
    reportDescription: ($) =>
      seq($.reportDescriptionEntry, repeat1($.reportGroupDescriptionEntry)),
    reportDescriptionEntry: ($) =>
      seq(
        $.RD,
        $.reportName,
        optional($.reportDescriptionGlobalClause),
        optional(
          seq(
            $.reportDescriptionPageLimitClause,
            optional($.reportDescriptionHeadingClause),
            optional($.reportDescriptionFirstDetailClause),
            optional($.reportDescriptionLastDetailClause),
            optional($.reportDescriptionFootingClause)
          )
        ),
        $.DOT_FS
      ),
    reportDescriptionGlobalClause: ($) => seq(optional($.IS), $.GLOBAL),
    reportDescriptionPageLimitClause: ($) =>
      seq(
        $.PAGE,
        optional(
          seq($.LIMIT, choice(optional($.IS), $.LIMITS), optional($.ARE))
        ),
        $.integerLiteral,
        optional(choice($.LINE, $.LINES))
      ),
    reportDescriptionHeadingClause: ($) => seq($.HEADING, $.integerLiteral),
    reportDescriptionFirstDetailClause: ($) =>
      seq($.FIRST, $.DETAIL, $.integerLiteral),
    reportDescriptionLastDetailClause: ($) =>
      seq($.LAST, $.DETAIL, $.integerLiteral),
    reportDescriptionFootingClause: ($) => seq($.FOOTING, $.integerLiteral),
    reportGroupDescriptionEntry: ($) =>
      choice(
        $.reportGroupDescriptionEntryFormat1,
        $.reportGroupDescriptionEntryFormat2,
        $.reportGroupDescriptionEntryFormat3
      ),
    reportGroupDescriptionEntryFormat1: ($) =>
      seq(
        $.integerLiteral,
        $.dataName,
        optional($.reportGroupLineNumberClause),
        optional($.reportGroupNextGroupClause),
        $.reportGroupTypeClause,
        optional($.reportGroupUsageClause),
        $.DOT_FS
      ),
    reportGroupDescriptionEntryFormat2: ($) =>
      seq(
        $.integerLiteral,
        optional($.dataName),
        optional($.reportGroupLineNumberClause),
        $.reportGroupUsageClause,
        $.DOT_FS
      ),
    reportGroupDescriptionEntryFormat3: ($) =>
      seq(
        $.integerLiteral,
        optional($.dataName),
        repeat(
          choice(
            $.reportGroupPictureClause,
            $.reportGroupUsageClause,
            $.reportGroupSignClause,
            $.reportGroupJustifiedClause,
            $.reportGroupBlankWhenZeroClause,
            $.reportGroupLineNumberClause,
            $.reportGroupColumnNumberClause,
            $.reportGroupSourceClause,
            $.reportGroupValueClause,
            $.reportGroupSumClause,
            $.reportGroupResetClause,
            $.reportGroupIndicateClause
          )
        ),
        $.DOT_FS
      ),
    reportGroupBlankWhenZeroClause: ($) =>
      seq($.BLANK, optional($.WHEN), $.ZERO),
    reportGroupColumnNumberClause: ($) =>
      seq($.COLUMN, optional($.NUMBER), optional($.IS), $.integerLiteral),
    reportGroupIndicateClause: ($) => seq($.GROUP, optional($.INDICATE)),
    reportGroupJustifiedClause: ($) =>
      seq(choice($.JUSTIFIED, $.JUST), optional($.RIGHT)),
    reportGroupLineNumberClause: ($) =>
      seq(
        optional($.LINE),
        optional($.NUMBER),
        optional($.IS),
        choice($.reportGroupLineNumberNextPage, $.reportGroupLineNumberPlus)
      ),
    reportGroupLineNumberNextPage: ($) =>
      seq($.integerLiteral, optional(seq(optional($.ON), $.NEXT, $.PAGE))),
    reportGroupLineNumberPlus: ($) => seq($.PLUS, $.integerLiteral),
    reportGroupNextGroupClause: ($) =>
      seq(
        $.NEXT,
        $.GROUP,
        optional($.IS),
        choice(
          $.integerLiteral,
          $.reportGroupNextGroupNextPage,
          $.reportGroupNextGroupPlus
        )
      ),
    reportGroupNextGroupPlus: ($) => seq($.PLUS, $.integerLiteral),
    reportGroupNextGroupNextPage: ($) => seq($.NEXT, $.PAGE),
    reportGroupPictureClause: ($) =>
      seq(choice($.PICTURE, $.PIC), optional($.IS), $.pictureString),
    reportGroupResetClause: ($) =>
      seq($.RESET, optional($.ON), choice($.FINAL, $.dataName)),
    reportGroupSignClause: ($) =>
      seq(
        $.SIGN,
        optional($.IS),
        choice($.LEADING, $.TRAILING),
        $.SEPARATE,
        optional($.CHARACTER)
      ),
    reportGroupSourceClause: ($) => seq($.SOURCE, optional($.IS), $.identifier),
    reportGroupSumClause: ($) =>
      seq(
        $.SUM,
        $.identifier,
        repeat(seq(optional($.COMMACHAR), $.identifier)),
        optional(
          seq(
            $.UPON,
            $.dataName,
            repeat(seq(optional($.COMMACHAR), $.dataName))
          )
        )
      ),
    reportGroupTypeClause: ($) =>
      seq(
        $.TYPE,
        optional($.IS),
        choice(
          $.reportGroupTypeReportHeading,
          $.reportGroupTypePageHeading,
          $.reportGroupTypeControlHeading,
          $.reportGroupTypeDetail,
          $.reportGroupTypeControlFooting,
          $.reportGroupTypePageFooting,
          $.reportGroupTypeReportFooting
        )
      ),
    reportGroupTypeReportHeading: ($) => choice(seq($.REPORT, $.HEADING), $.RH),
    reportGroupTypePageHeading: ($) => choice(seq($.PAGE, $.HEADING), $.PH),
    reportGroupTypeControlHeading: ($) =>
      seq(choice(seq($.CONTROL, $.HEADING), $.CH), choice($.FINAL, $.dataName)),
    reportGroupTypeDetail: ($) => choice($.DETAIL, $.DE),
    reportGroupTypeControlFooting: ($) =>
      seq(choice(seq($.CONTROL, $.FOOTING), $.CF), choice($.FINAL, $.dataName)),
    reportGroupUsageClause: ($) =>
      seq(
        optional(seq($.USAGE, optional($.IS))),
        choice($.DISPLAY, $.DISPLAY_1)
      ),
    reportGroupTypePageFooting: ($) => choice(seq($.PAGE, $.FOOTING), $.PF),
    reportGroupTypeReportFooting: ($) => choice(seq($.REPORT, $.FOOTING), $.RF),
    reportGroupValueClause: ($) => seq($.VALUE, optional($.IS), $.literal),
    // -- program library section ----------------------------------
    programLibrarySection: ($) =>
      seq(
        $.PROGRAM_LIBRARY,
        $.SECTION,
        $.DOT_FS,
        repeat($.libraryDescriptionEntry)
      ),
    libraryDescriptionEntry: ($) =>
      choice(
        $.libraryDescriptionEntryFormat1,
        $.libraryDescriptionEntryFormat2
      ),
    libraryDescriptionEntryFormat1: ($) =>
      seq(
        $.LD,
        $.libraryName,
        $.EXPORT,
        optional($.libraryAttributeClauseFormat1),
        optional($.libraryEntryProcedureClauseFormat1)
      ),
    libraryDescriptionEntryFormat2: ($) =>
      seq(
        $.LB,
        $.libraryName,
        $.IMPORT,
        optional($.libraryIsGlobalClause),
        optional($.libraryIsCommonClause),
        repeat(
          choice(
            $.libraryAttributeClauseFormat2,
            $.libraryEntryProcedureClauseFormat2
          )
        )
      ),
    libraryAttributeClauseFormat1: ($) =>
      seq(
        $.ATTRIBUTE,
        optional(
          seq(
            $.SHARING,
            optional($.IS),
            seq(choice($.DONTCARE, $.PRIVATE, $.SHAREDBYRUNUNIT, $.SHAREDBYALL))
          )
        )
      ),
    libraryAttributeClauseFormat2: ($) =>
      seq(
        $.ATTRIBUTE,
        optional($.libraryAttributeFunction),
        optional(
          seq($.LIBACCESS, optional($.IS), choice($.BYFUNCTION, $.BYTITLE))
        ),
        optional($.libraryAttributeParameter),
        optional($.libraryAttributeTitle)
      ),
    libraryAttributeFunction: ($) => seq($.FUNCTIONNAME, $.IS, $.literal),
    libraryAttributeParameter: ($) =>
      seq($.LIBPARAMETER, optional($.IS), $.literal),
    libraryAttributeTitle: ($) => seq($.TITLE, optional($.IS), $.literal),
    libraryEntryProcedureClauseFormat1: ($) =>
      seq(
        $.ENTRY_PROCEDURE,
        $.programName,
        optional($.libraryEntryProcedureForClause)
      ),
    libraryEntryProcedureClauseFormat2: ($) =>
      seq(
        $.ENTRY_PROCEDURE,
        $.programName,
        optional($.libraryEntryProcedureForClause),
        optional($.libraryEntryProcedureWithClause),
        optional($.libraryEntryProcedureUsingClause),
        optional($.libraryEntryProcedureGivingClause)
      ),
    libraryEntryProcedureForClause: ($) => seq($.FOR, $.literal),
    libraryEntryProcedureGivingClause: ($) => seq($.GIVING, $.dataName),
    libraryEntryProcedureUsingClause: ($) =>
      seq($.USING, repeat1($.libraryEntryProcedureUsingName)),
    libraryEntryProcedureUsingName: ($) => choice($.dataName, $.fileName),
    libraryEntryProcedureWithClause: ($) =>
      seq($.WITH, repeat1($.libraryEntryProcedureWithName)),
    libraryEntryProcedureWithName: ($) => choice($.localName, $.fileName),
    libraryIsCommonClause: ($) => seq(optional($.IS), $.COMMON),
    libraryIsGlobalClause: ($) => seq(optional($.IS), $.GLOBAL),
    // data description entry ----------------------------------
    dataDescriptionEntry: ($) =>
      choice(
        $.dataDescriptionEntryFormat1,
        $.dataDescriptionEntryFormat2,
        $.dataDescriptionEntryFormat3,
        $.dataDescriptionEntryExecSql
      ),
    dataDescriptionEntryFormat1: ($) =>
      seq(
        choice($.INTEGERLITERAL, $.LEVEL_NUMBER_77),
        optional(choice($.FILLER, $.dataName)),
        repeat(
          choice(
            $.dataRedefinesClause,
            $.dataIntegerStringClause,
            $.dataExternalClause,
            $.dataGlobalClause,
            $.dataTypeDefClause,
            $.dataThreadLocalClause,
            $.dataPictureClause,
            $.dataCommonOwnLocalClause,
            $.dataTypeClause,
            $.dataUsingClause,
            $.dataUsageClause,
            $.dataValueClause,
            $.dataReceivedByClause,
            $.dataOccursClause,
            $.dataSignClause,
            $.dataSynchronizedClause,
            $.dataJustifiedClause,
            $.dataBlankWhenZeroClause,
            $.dataWithLowerBoundsClause,
            $.dataAlignedClause,
            $.dataRecordAreaClause
          )
        ),
        $.DOT_FS
      ),
    dataDescriptionEntryFormat2: ($) =>
      seq($.LEVEL_NUMBER_66, $.dataName, $.dataRenamesClause, $.DOT_FS),
    dataDescriptionEntryFormat3: ($) =>
      seq($.LEVEL_NUMBER_88, $.conditionName, $.dataValueClause, $.DOT_FS),
    dataDescriptionEntryExecSql: ($) =>
      seq(repeat1($.EXECSQLLINE), optional($.DOT_FS)),
    dataAlignedClause: ($) => $.ALIGNED,
    dataBlankWhenZeroClause: ($) =>
      seq($.BLANK, optional($.WHEN), choice($.ZERO, $.ZEROS, $.ZEROES)),
    dataCommonOwnLocalClause: ($) => seq(choice($.COMMON, $.OWN, $.LOCAL)),
    dataExternalClause: ($) =>
      seq(optional($.IS), $.EXTERNAL, optional(seq($.BY, $.literal))),
    dataGlobalClause: ($) => seq(optional($.IS), $.GLOBAL),
    dataIntegerStringClause: ($) => choice($.INTEGER, $.STRING),
    dataJustifiedClause: ($) =>
      seq(choice($.JUSTIFIED, $.JUST), optional($.RIGHT)),
    dataOccursClause: ($) =>
      seq(
        $.OCCURS,
        choice($.identifier, $.integerLiteral),
        optional($.dataOccursTo),
        optional($.TIMES),
        optional($.dataOccursDepending),
        repeat(choice($.dataOccursSort, $.dataOccursIndexed))
      ),
    dataOccursTo: ($) => seq($.TO, $.integerLiteral),
    dataOccursDepending: ($) =>
      seq($.DEPENDING, optional($.ON), $.qualifiedDataName),
    dataOccursSort: ($) =>
      seq(
        choice($.ASCENDING, $.DESCENDING),
        optional($.KEY),
        optional($.IS),
        repeat1($.qualifiedDataName)
      ),
    dataOccursIndexed: ($) =>
      seq($.INDEXED, optional($.BY), optional($.LOCAL), repeat1($.indexName)),
    dataPictureClause: ($) =>
      seq(choice($.PICTURE, $.PIC), optional($.IS), $.pictureString),
    pictureString: ($) =>
      repeat1(seq(repeat1($.pictureChars), optional($.pictureCardinality))),
    pictureChars: ($) =>
      prec(
        2,
        choice(
          $.DOLLARCHAR,
          $.IDENTIFIER,
          $.NUMERICLITERAL,
          $.SLASHCHAR,
          $.COMMACHAR,
          $.DOT,
          $.COLONCHAR,
          $.ASTERISKCHAR,
          $.DOUBLEASTERISKCHAR,
          $.LPARENCHAR,
          $.RPARENCHAR,
          $.PLUSCHAR,
          $._MINUSCHAR,
          $.LESSTHANCHAR,
          $.MORETHANCHAR,
          $.integerLiteral
        )
      ),
    pictureCardinality: ($) =>
      seq($.LPARENCHAR, $.integerLiteral, $.RPARENCHAR),
    dataReceivedByClause: ($) =>
      seq(
        optional($.RECEIVED),
        optional($.BY),
        choice($.CONTENT, $.REFERENCE, $.REF)
      ),
    dataRecordAreaClause: ($) => seq($.RECORD, $.AREA),
    dataRedefinesClause: ($) => seq($.REDEFINES, $.dataName),
    dataRenamesClause: ($) =>
      seq(
        $.RENAMES,
        $.qualifiedDataName,
        optional(seq(choice($.THROUGH, $.THRU), $.qualifiedDataName))
      ),
    dataSignClause: ($) =>
      seq(
        optional(seq($.SIGN, optional($.IS))),
        choice($.LEADING, $.TRAILING),
        optional(seq($.SEPARATE, optional($.CHARACTER)))
      ),
    dataSynchronizedClause: ($) =>
      seq(choice($.SYNCHRONIZED, $.SYNC), optional(choice($.LEFT, $.RIGHT))),
    dataThreadLocalClause: ($) => seq(optional($.IS), $.THREAD_LOCAL),
    dataTypeClause: ($) =>
      seq(
        $.TYPE,
        optional($.IS),
        choice(
          $.SHORT_DATE,
          $.LONG_DATE,
          $.NUMERIC_DATE,
          $.NUMERIC_TIME,
          $.LONG_TIME,
          seq(
            choice($.CLOB, $.BLOB, $.DBCLOB),
            $.LPARENCHAR,
            $.integerLiteral,
            $.RPARENCHAR
          )
        )
      ),
    dataTypeDefClause: ($) => seq(optional($.IS), $.TYPEDEF),
    dataUsageClause: ($) =>
      seq(
        optional(seq($.USAGE, optional($.IS))),
        choice(
          seq($.BINARY, optional(choice($.TRUNCATED, $.EXTENDED))),
          $.BIT,
          $.COMP,
          $.COMP_1,
          $.COMP_2,
          $.COMP_3,
          $.COMP_4,
          $.COMP_5,
          $.COMPUTATIONAL,
          $.COMPUTATIONAL_1,
          $.COMPUTATIONAL_2,
          $.COMPUTATIONAL_3,
          $.COMPUTATIONAL_4,
          $.COMPUTATIONAL_5,
          $.CONTROL_POINT,
          $.DATE,
          $.DISPLAY,
          $.DISPLAY_1,
          $.DOUBLE,
          $.EVENT,
          $.FUNCTION_POINTER,
          $.INDEX,
          $.KANJI,
          $.LOCK,
          $.NATIONAL,
          $.PACKED_DECIMAL,
          $.POINTER,
          $.PROCEDURE_POINTER,
          $.REAL,
          $.SQL,
          $.TASK
        )
      ),
    dataUsingClause: ($) =>
      seq(
        $.USING,
        choice($.LANGUAGE, $.CONVENTION),
        optional($.OF),
        choice($.cobolWord, $.dataName)
      ),
    dataValueClause: ($) =>
      seq(
        optional(seq(choice($.VALUE, $.VALUES), optional(choice($.IS, $.ARE)))),
        $.dataValueInterval,
        repeat(seq(optional($.COMMACHAR), $.dataValueInterval))
      ),
    dataValueInterval: ($) =>
      seq($.dataValueIntervalFrom, optional($.dataValueIntervalTo)),
    dataValueIntervalFrom: ($) => choice($.literal, $.cobolWord),
    dataValueIntervalTo: ($) => seq(choice($.THROUGH, $.THRU), $.literal),
    dataWithLowerBoundsClause: ($) => seq(optional($.WITH), $.LOWER, $.BOUNDS),
    // --- procedure division --------------------------------------------------------------------
    procedureDivision: ($) =>
      seq(
        $.PROCEDURE,
        $.DIVISION,
        optional($.procedureDivisionUsingClause),
        optional($.procedureDivisionGivingClause),
        $.DOT_FS,
        optional($.procedureDeclaratives),
        $.procedureDivisionBody
      ),
    procedureDivisionUsingClause: ($) =>
      seq(
        choice($.USING, $.CHAINING),
        repeat1($.procedureDivisionUsingParameter)
      ),
    procedureDivisionGivingClause: ($) =>
      seq(choice($.GIVING, $.RETURNING), $.dataName),
    procedureDivisionUsingParameter: ($) =>
      choice(
        $.procedureDivisionByReferencePhrase,
        $.procedureDivisionByValuePhrase
      ),
    procedureDivisionByReferencePhrase: ($) =>
      seq(
        optional(seq(optional($.BY), $.REFERENCE)),
        repeat1($.procedureDivisionByReference)
      ),
    procedureDivisionByReference: ($) =>
      //(OPTIONAL? (identifier | $.fileName)) | ANY
      choice(
        seq(optional($.OPTIONAL), choice($.identifier, $.fileName)),
        $.ANY
      ),
    procedureDivisionByValuePhrase: ($) =>
      seq(optional($.BY), $.VALUE, repeat1($.procedureDivisionByValue)),
    procedureDivisionByValue: ($) => choice($.identifier, $.literal, $.ANY),
    procedureDeclaratives: ($) =>
      seq(
        $.DECLARATIVES,
        $.DOT_FS,
        repeat1($.procedureDeclarative),
        $.END,
        $.DECLARATIVES,
        $.DOT_FS
      ),
    procedureDeclarative: ($) =>
      seq(
        $.procedureSectionHeader,
        $.DOT_FS,
        $.useStatement,
        $.DOT_FS,
        $.paragraphs
      ),
    procedureSectionHeader: ($) =>
      seq($.sectionName, $.SECTION, optional($.integerLiteral)),
    procedureDivisionBody: ($) => seq($.paragraphs, repeat($.procedureSection)),
    // -- procedure section ----------------------------------
    procedureSection: ($) =>
      seq($.procedureSectionHeader, $.DOT_FS, $.paragraphs),
    paragraphs: ($) => seq(repeat($.sentence), repeat1($.paragraph)),
    paragraph: ($) =>
      seq(
        $.paragraphName,
        optional($.DOT_FS),
        choice($.alteredGoTo, repeat($.sentence))
      ),
    sentence: ($) => seq(repeat($.statement), $.DOT_FS),
    statement: ($) =>
      choice(
        $.acceptStatement,
        $.addStatement,
        $.alterStatement,
        $.callStatement,
        $.cancelStatement,
        $.closeStatement,
        $.computeStatement,
        $.continueStatement,
        $.deleteStatement,
        $.disableStatement,
        $.displayStatement,
        $.divideStatement,
        $.enableStatement,
        $.entryStatement,
        $.evaluateStatement,
        $.exhibitStatement,
        $.execCicsStatement,
        $.execSqlStatement,
        $.execSqlImsStatement,
        $.exitStatement,
        $.generateStatement,
        $.gobackStatement,
        $.goToStatement,
        $.ifStatement,
        $.initializeStatement,
        $.initiateStatement,
        $.inspectStatement,
        $.mergeStatement,
        $.moveStatement,
        $.multiplyStatement,
        $.nextSentenceStatement,
        $.openStatement,
        $.performStatement,
        $.purgeStatement,
        $.readStatement,
        $.receiveStatement,
        $.releaseStatement,
        $.returnStatement,
        $.rewriteStatement,
        $.searchStatement,
        $.sendStatement,
        $.setStatement,
        $.sortStatement,
        $.startStatement,
        $.stopStatement,
        $.stringStatement,
        $.subtractStatement,
        $.terminateStatement,
        $.unstringStatement,
        $.writeStatement
      ),
    // accept statement
    acceptStatement: ($) =>
      seq(
        $.ACCEPT,
        $.identifier,
        optional(
          choice(
            $.acceptFromDateStatement,
            $.acceptFromEscapeKeyStatement,
            $.acceptFromMnemonicStatement,
            $.acceptMessageCountStatement
          )
        ),
        optional($.onExceptionClause),
        optional($.notOnExceptionClause),
        optional($.END_ACCEPT)
      ),
    acceptFromDateStatement: ($) =>
      seq(
        $.FROM,
        seq(
          $.DATE,
          choice(optional($.YYYYMMDD), $.DAY),
          choice(
            optional($.YYYYDDD),
            $.DAY_OF_WEEK,
            $.TIME,
            $.TIMER,
            $.TODAYS_DATE
          ),
          choice(
            optional($.MMDDYYYY),
            $.TODAYS_NAME,
            $.YEAR,
            $.YYYYMMDD,
            $.YYYYDDD
          )
        )
      ),
    acceptFromMnemonicStatement: ($) => seq($.FROM, $.mnemonicName),
    acceptFromEscapeKeyStatement: ($) => seq($.FROM, $.ESCAPE, $.KEY),
    acceptMessageCountStatement: ($) => seq(optional($.MESSAGE), $.COUNT),
    // add statement
    addStatement: ($) =>
      seq(
        $.ADD,
        choice(
          $.addToStatement,
          $.addToGivingStatement,
          $.addCorrespondingStatement
        ),
        optional($.onSizeErrorPhrase),
        optional($.notOnSizeErrorPhrase),
        optional($.END_ADD)
      ),
    addToStatement: ($) => seq(repeat1($.addFrom), $.TO, repeat1($.addTo)),
    addToGivingStatement: ($) =>
      seq(
        repeat1($.addFrom),
        optional(seq($.TO, repeat1($.addToGiving))),
        $.GIVING,
        repeat1($.addGiving)
      ),
    addCorrespondingStatement: ($) =>
      seq(choice($.CORRESPONDING, $.CORR), $.identifier, $.TO, $.addTo),
    addFrom: ($) => choice($.identifier, $.literal),
    addTo: ($) => seq($.identifier, optional($.ROUNDED)),
    addToGiving: ($) => choice($.identifier, $.literal),
    addGiving: ($) => seq($.identifier, optional($.ROUNDED)),
    // altered go to statement
    alteredGoTo: ($) => seq($.GO, optional($.TO), $.DOT_FS),
    // alter statement
    alterStatement: ($) => seq($.ALTER, repeat1($.alterProceedTo)),
    alterProceedTo: ($) =>
      seq(
        $.procedureName,
        $.TO,
        optional(seq($.PROCEED, $.TO)),
        $.procedureName
      ),
    // call statement
    callStatement: ($) =>
      seq(
        $.CALL,
        choice($.identifier, $.literal),
        optional($.callUsingPhrase),
        optional($.callGivingPhrase),
        optional($.onOverflowPhrase),
        optional($.onExceptionClause),
        optional($.notOnExceptionClause),
        optional($.END_CALL)
      ),
    callUsingPhrase: ($) => seq($.USING, repeat1($.callUsingParameter)),
    callUsingParameter: ($) =>
      choice(
        $.callByReferencePhrase,
        $.callByValuePhrase,
        $.callByContentPhrase
      ),
    callByReferencePhrase: ($) =>
      seq(
        optional(seq(optional($.BY), $.REFERENCE)),
        repeat1($.callByReference)
      ),
    callByReference: ($) =>
      choice(
        choice(
          seq(
            optional(choice(seq($.ADDRESS, $.OF), $.INTEGER, $.STRING)),
            $.identifier
          ),
          $.literal,
          $.fileName
        ),
        $.OMITTED
      ),
    callByValuePhrase: ($) =>
      seq(optional($.BY), $.VALUE, repeat1($.callByValue)),
    callByValue: ($) =>
      seq(
        optional(seq($.ADDRESS, choice($.OF, $.LENGTH), optional($.OF))),
        choice($.identifier, $.literal)
      ),
    callByContentPhrase: ($) =>
      seq(optional($.BY), $.CONTENT, repeat1($.callByContent)),
    callByContent: ($) =>
      seq(
        optional(choice(seq($.ADDRESS, $.OF), seq($.LENGTH, optional($.OF)))),
        choice($.identifier, $.literal, $.OMITTED)
      ),
    callGivingPhrase: ($) => seq(choice($.GIVING, $.RETURNING), $.identifier),
    // cancel statement
    cancelStatement: ($) => seq($.CANCEL, repeat1($.cancelCall)),
    cancelCall: ($) =>
      seq(
        $.libraryName,
        choice($.BYTITLE, $.BYFUNCTION, $.identifier, $.literal)
      ),
    // close statement
    closeStatement: ($) => seq($.CLOSE, repeat1($.closeFile)),
    closeFile: ($) =>
      seq(
        $.fileName,
        optional(
          choice(
            $.closeReelUnitStatement,
            $.closeRelativeStatement,
            $.closePortFileIOStatement
          )
        )
      ),
    closeReelUnitStatement: ($) =>
      seq(
        choice($.REEL, $.UNIT),
        optional(seq(optional($.FOR), $.REMOVAL)),
        optional(seq(optional($.WITH), choice(seq($.NO, $.REWIND), $.LOCK)))
      ),
    closeRelativeStatement: ($) =>
      seq(optional($.WITH), choice(seq($.NO, $.REWIND), $.LOCK)),
    closePortFileIOStatement: ($) =>
      seq(
        choice(seq(optional($.WITH), $.NO, $.WAIT), seq($.WITH, $.WAIT)),
        optional(seq($.USING, repeat1($.closePortFileIOUsing)))
      ),
    closePortFileIOUsing: ($) =>
      choice(
        $.closePortFileIOUsingCloseDisposition,
        $.closePortFileIOUsingAssociatedData,
        $.closePortFileIOUsingAssociatedDataLength
      ),
    closePortFileIOUsingCloseDisposition: ($) =>
      seq($.CLOSE_DISPOSITION, optional($.OF), choice($.ABORT, $.ORDERLY)),
    closePortFileIOUsingAssociatedData: ($) =>
      seq($.ASSOCIATED_DATA, choice($.identifier, $.integerLiteral)),
    closePortFileIOUsingAssociatedDataLength: ($) =>
      seq(
        $.ASSOCIATED_DATA_LENGTH,
        optional($.OF),
        choice($.identifier, $.integerLiteral)
      ),
    // compute statement
    computeStatement: ($) =>
      seq(
        $.COMPUTE,
        repeat1($.computeStore),
        choice($.EQUALCHAR, $.EQUAL),
        $.arithmeticExpression,
        optional($.onSizeErrorPhrase),
        optional($.notOnSizeErrorPhrase),
        optional($.END_COMPUTE)
      ),
    computeStore: ($) => seq($.identifier, optional($.ROUNDED)),
    // continue statement
    continueStatement: ($) => $.CONTINUE,
    // delete statement
    deleteStatement: ($) =>
      seq(
        $.DELETE,
        $.fileName,
        optional($.RECORD),
        optional($.invalidKeyPhrase),
        optional($.notInvalidKeyPhrase),
        optional($.END_DELETE)
      ),
    // disable statement
    disableStatement: ($) =>
      seq(
        $.DISABLE,
        choice(
          seq($.INPUT, optional($.TERMINAL)),
          seq($.I_O, $.TERMINAL),
          $.OUTPUT
        ),
        $.cdName,
        optional($.WITH),
        $.KEY,
        choice($.identifier, $.literal)
      ),
    // display statement
    displayStatement: ($) =>
      seq(
        $.DISPLAY,
        repeat1($.displayOperand),
        optional($.displayAt),
        optional($.displayUpon),
        optional($.displayWith),
        optional($.onExceptionClause),
        optional($.notOnExceptionClause),
        optional($.END_DISPLAY)
      ),
    displayOperand: ($) => choice($.identifier, $.literal),
    displayAt: ($) => seq($.AT, choice($.identifier, $.literal)),
    displayUpon: ($) => seq($.UPON, choice($.mnemonicName, $.environmentName)),
    displayWith: ($) => seq(optional($.WITH), $.NO, $.ADVANCING),
    // divide statement
    divideStatement: ($) =>
      seq(
        $.DIVIDE,
        choice($.identifier, $.literal),
        choice(
          $.divideIntoStatement,
          $.divideIntoGivingStatement,
          $.divideByGivingStatement
        ),
        optional($.divideRemainder),
        optional($.onSizeErrorPhrase),
        optional($.notOnSizeErrorPhrase),
        optional($.END_DIVIDE)
      ),
    divideIntoStatement: ($) => seq($.INTO, repeat1($.divideInto)),
    divideIntoGivingStatement: ($) =>
      seq(
        $.INTO,
        choice($.identifier, $.literal),
        optional($.divideGivingPhrase)
      ),
    divideByGivingStatement: ($) =>
      seq(
        $.BY,
        choice($.identifier, $.literal),
        optional($.divideGivingPhrase)
      ),
    divideGivingPhrase: ($) => seq($.GIVING, repeat1($.divideGiving)),
    divideInto: ($) => seq($.identifier, optional($.ROUNDED)),
    divideGiving: ($) => seq($.identifier, optional($.ROUNDED)),
    divideRemainder: ($) => seq($.REMAINDER, $.identifier),
    // enable statement
    enableStatement: ($) =>
      seq(
        $.ENABLE,
        choice(
          seq($.INPUT, optional($.TERMINAL)),
          seq($.I_O, $.TERMINAL),
          $.OUTPUT
        ),
        $.cdName,
        optional($.WITH),
        $.KEY,
        choice($.literal, $.identifier)
      ),
    // entry statement
    entryStatement: ($) =>
      seq($.ENTRY, $.literal, optional(seq($.USING, repeat1($.identifier)))),
    // evaluate statement
    evaluateStatement: ($) =>
      seq(
        $.EVALUATE,
        $.evaluateSelect,
        repeat($.evaluateAlsoSelect),
        repeat($.evaluateWhenPhrase),
        optional($.evaluateWhenOther),
        optional($.END_EVALUATE)
      ),
    evaluateSelect: ($) =>
      choice($.identifier, $.literal, $.arithmeticExpression, $.condition),
    evaluateAlsoSelect: ($) => seq($.ALSO, $.evaluateSelect),
    evaluateWhenPhrase: ($) =>
      seq(repeat1($.evaluateWhen), repeat($.statement)),
    evaluateWhen: ($) =>
      seq($.WHEN, $.evaluateCondition, repeat($.evaluateAlsoCondition)),
    evaluateCondition: ($) =>
      seq(
        choice($.ANY, optional($.NOT)),
        $.evaluateValue,
        choice(optional($.evaluateThrough), $.condition, $.booleanLiteral)
      ),
    evaluateThrough: ($) => seq(choice($.THROUGH, $.THRU), $.evaluateValue),
    evaluateAlsoCondition: ($) => seq($.ALSO, $.evaluateCondition),
    evaluateWhenOther: ($) => seq($.WHEN, $.OTHER, repeat($.statement)),
    evaluateValue: ($) =>
      choice($.identifier, $.literal, $.arithmeticExpression),
    // exec cics statement
    execCicsStatement: ($) => repeat1($.EXECCICSLINE),
    // exec sql statement
    execSqlStatement: ($) => repeat1($.EXECSQLLINE),
    // exec sql ims statement
    execSqlImsStatement: ($) => repeat1($.EXECSQLIMSLINE),
    // exhibit statement
    exhibitStatement: ($) =>
      prec(
        2,
        seq(
          $.EXHIBIT,
          optional($.NAMED),
          optional($.CHANGED),
          repeat1($.exhibitOperand)
        )
      ),
    exhibitOperand: ($) => choice($.identifier, $.literal),
    // exit statement
    exitStatement: ($) => seq($.EXIT, optional($.PROGRAM)),
    // generate statement
    generateStatement: ($) => seq($.GENERATE, $.reportName),
    // goback statement
    gobackStatement: ($) => $.GOBACK,
    // goto statement
    goToStatement: ($) =>
      seq(
        $.GO,
        optional($.TO),
        choice($.goToStatementSimple, $.goToDependingOnStatement)
      ),
    goToStatementSimple: ($) => $.procedureName,
    goToDependingOnStatement: ($) =>
      seq(
        choice($.MORE_LABELS, repeat1($.procedureName)),
        optional(seq($.DEPENDING, optional($.ON), $.identifier))
      ),
    // if statement
    ifStatement: ($) =>
      seq($.IF, $.condition, $.ifThen, optional($.ifElse), optional($.END_IF)),
    ifThen: ($) =>
      seq(
        optional($.THEN),
        choice(seq($.NEXT, $.SENTENCE), repeat1($.statement))
      ),
    ifElse: ($) =>
      seq($.ELSE, choice(seq($.NEXT, $.SENTENCE), repeat($.statement))),
    // initialize statement
    initializeStatement: ($) =>
      seq(
        $.INITIALIZE,
        repeat1($.identifier),
        optional($.initializeReplacingPhrase)
      ),
    initializeReplacingPhrase: ($) =>
      seq($.REPLACING, repeat1($.initializeReplacingBy)),
    initializeReplacingBy: ($) =>
      seq(
        choice(
          $.ALPHABETIC,
          $.ALPHANUMERIC,
          $.ALPHANUMERIC_EDITED,
          $.NATIONAL,
          $.NATIONAL_EDITED,
          $.NUMERIC,
          $.NUMERIC_EDITED,
          $.DBCS,
          $.EGCS
        ),
        optional($.DATA),
        $.BY,
        choice($.identifier, $.literal)
      ),
    // initiate statement
    initiateStatement: ($) => seq($.INITIATE, repeat1($.reportName)),
    // inspect statement
    inspectStatement: ($) =>
      seq(
        $.INSPECT,
        $.identifier,
        choice(
          $.inspectTallyingPhrase,
          $.inspectReplacingPhrase,
          $.inspectTallyingReplacingPhrase,
          $.inspectConvertingPhrase
        )
      ),
    inspectTallyingPhrase: ($) => seq($.TALLYING, repeat1($.inspectFor)),
    inspectReplacingPhrase: ($) =>
      seq(
        $.REPLACING,
        repeat1(
          choice($.inspectReplacingCharacters, $.inspectReplacingAllLeadings)
        )
      ),
    inspectTallyingReplacingPhrase: ($) =>
      seq($.TALLYING, repeat1($.inspectFor), repeat1($.inspectReplacingPhrase)),
    inspectConvertingPhrase: ($) =>
      seq(
        $.CONVERTING,
        choice($.identifier, $.literal),
        $.inspectTo,
        repeat($.inspectBeforeAfter)
      ),
    inspectFor: ($) =>
      seq(
        $.identifier,
        $.FOR,
        repeat1(choice($.inspectCharacters, $.inspectAllLeadings))
      ),
    inspectCharacters: ($) =>
      seq(choice($.CHARACTER, $.CHARACTERS), repeat($.inspectBeforeAfter)),
    inspectReplacingCharacters: ($) =>
      seq(
        choice($.CHARACTER, $.CHARACTERS),
        $.inspectBy,
        repeat($.inspectBeforeAfter)
      ),
    inspectAllLeadings: ($) =>
      seq(choice($.ALL, $.LEADING), repeat1($.inspectAllLeading)),
    inspectReplacingAllLeadings: ($) =>
      seq(
        choice($.ALL, $.LEADING, $.FIRST),
        repeat1($.inspectReplacingAllLeading)
      ),
    inspectAllLeading: ($) =>
      seq(choice($.identifier, $.literal), repeat($.inspectBeforeAfter)),
    inspectReplacingAllLeading: ($) =>
      seq(
        choice($.identifier, $.literal),
        $.inspectBy,
        repeat($.inspectBeforeAfter)
      ),
    inspectBy: ($) => seq($.BY, choice($.identifier, $.literal)),
    inspectTo: ($) => seq($.TO, choice($.identifier, $.literal)),
    inspectBeforeAfter: ($) =>
      seq(
        choice($.BEFORE, $.AFTER),
        optional($.INITIAL),
        choice($.identifier, $.literal)
      ),
    // merge statement
    mergeStatement: ($) =>
      seq(
        $.MERGE,
        $.fileName,
        repeat1($.mergeOnKeyClause),
        optional($.mergeCollatingSequencePhrase),
        repeat($.mergeUsing),
        optional($.mergeOutputProcedurePhrase),
        repeat($.mergeGivingPhrase)
      ),
    mergeOnKeyClause: ($) =>
      seq(
        optional($.ON),
        choice($.ASCENDING, $.DESCENDING),
        optional($.KEY),
        repeat1($.qualifiedDataName)
      ),
    mergeCollatingSequencePhrase: ($) =>
      seq(
        optional($.COLLATING),
        $.SEQUENCE,
        optional($.IS),
        repeat1($.alphabetName),
        optional($.mergeCollatingAlphanumeric),
        optional($.mergeCollatingNational)
      ),
    mergeCollatingAlphanumeric: ($) =>
      seq(optional($.FOR), $.ALPHANUMERIC, $.IS, $.alphabetName),
    mergeCollatingNational: ($) =>
      seq(optional($.FOR), $.NATIONAL, optional($.IS), $.alphabetName),
    mergeUsing: ($) => seq($.USING, repeat1($.fileName)),
    mergeOutputProcedurePhrase: ($) =>
      seq(
        $.OUTPUT,
        $.PROCEDURE,
        optional($.IS),
        $.procedureName,
        optional($.mergeOutputThrough)
      ),
    mergeOutputThrough: ($) => seq(choice($.THROUGH, $.THRU), $.procedureName),
    mergeGivingPhrase: ($) => seq($.GIVING, repeat1($.mergeGiving)),
    mergeGiving: ($) =>
      seq(
        $.fileName,
        optional(
          seq(
            choice($.LOCK, $.SAVE, $.NO),
            choice($.REWIND, $.CRUNCH, $.RELEASE, $.WITH),
            $.REMOVE,
            $.CRUNCH
          )
        )
      ),
    // move statement
    moveStatement: ($) =>
      seq(
        $.MOVE,
        optional($.ALL),
        choice($.moveToStatement, $.moveCorrespondingToStatement)
      ),
    moveToStatement: ($) =>
      seq($.moveToSendingArea, $.TO, repeat1($.identifier)),
    moveToSendingArea: ($) => choice($.identifier, $.literal),
    moveCorrespondingToStatement: ($) =>
      seq(
        choice($.CORRESPONDING, $.CORR),
        $.moveCorrespondingToSendingArea,
        $.TO,
        repeat1($.identifier)
      ),
    moveCorrespondingToSendingArea: ($) => $.identifier,
    // multiply statement
    multiplyStatement: ($) =>
      seq(
        $.MULTIPLY,
        choice($.identifier, $.literal),
        $.BY,
        choice($.multiplyRegular, $.multiplyGiving),
        optional($.onSizeErrorPhrase),
        optional($.notOnSizeErrorPhrase),
        optional($.END_MULTIPLY)
      ),
    multiplyRegular: ($) => repeat1($.multiplyRegularOperand),
    multiplyRegularOperand: ($) => seq($.identifier, optional($.ROUNDED)),
    multiplyGiving: ($) =>
      seq($.multiplyGivingOperand, $.GIVING, repeat1($.multiplyGivingResult)),
    multiplyGivingOperand: ($) => choice($.identifier, $.literal),
    multiplyGivingResult: ($) => seq($.identifier, optional($.ROUNDED)),
    // next sentence
    nextSentenceStatement: ($) => seq($.NEXT, $.SENTENCE),
    // open statement
    openStatement: ($) =>
      seq(
        $.OPEN,
        repeat1(
          choice(
            $.openInputStatement,
            $.openOutputStatement,
            $.openIOStatement,
            $.openExtendStatement
          )
        )
      ),
    openInputStatement: ($) => seq($.INPUT, repeat1($.openInput)),
    openInput: ($) =>
      seq(
        $.fileName,
        choice($.REVERSED, seq(optional($.WITH), $.NO, $.REWIND))
      ),
    openOutputStatement: ($) => seq($.OUTPUT, repeat1($.openOutput)),
    openOutput: ($) =>
      seq($.fileName, optional(seq(optional($.WITH), $.NO, $.REWIND))),
    openIOStatement: ($) => seq($.I_O, repeat1($.fileName)),
    openExtendStatement: ($) => seq($.EXTEND, repeat1($.fileName)),
    // perform statement
    performStatement: ($) =>
      seq(
        $.PERFORM,
        choice($.performInlineStatement, $.performProcedureStatement)
      ),
    performInlineStatement: ($) =>
      seq(optional($.performType), repeat($.statement), $.END_PERFORM),
    performProcedureStatement: ($) =>
      seq(
        $.procedureName,
        optional(seq(choice($.THROUGH, $.THRU), $.procedureName)),
        optional($.performType)
      ),
    performType: ($) =>
      choice($.performTimes, $.performUntil, $.performVarying),
    performTimes: ($) => seq(choice($.identifier, $.integerLiteral), $.TIMES),
    performUntil: ($) =>
      seq(optional($.performTestClause), $.UNTIL, $.condition),
    performVarying: ($) =>
      seq(
        $.performTestClause,
        choice($.performVaryingClause, $.performVaryingClause),
        optional($.performTestClause)
      ),
    performVaryingClause: ($) =>
      seq($.VARYING, $.performVaryingPhrase, repeat($.performAfter)),
    performVaryingPhrase: ($) =>
      seq(
        choice($.identifier, $.literal),
        $.performFrom,
        $.performBy,
        $.performUntil
      ),
    performAfter: ($) => seq($.AFTER, $.performVaryingPhrase),
    performFrom: ($) =>
      seq($.FROM, choice($.identifier, $.literal, $.arithmeticExpression)),
    performBy: ($) =>
      seq($.BY, choice($.identifier, $.literal, $.arithmeticExpression)),
    performTestClause: ($) =>
      seq(optional($.WITH), $.TEST, choice($.BEFORE, $.AFTER)),
    // purge statement
    purgeStatement: ($) => seq($.PURGE, repeat1($.cdName)),
    // read statement
    readStatement: ($) =>
      seq(
        $.READ,
        $.fileName,
        optional($.NEXT),
        optional($.RECORD),
        optional($.readInto),
        optional($.readWith),
        optional($.readKey),
        optional($.invalidKeyPhrase),
        optional($.notInvalidKeyPhrase),
        optional($.atEndPhrase),
        optional($.notAtEndPhrase),
        optional($.END_READ)
      ),
    readInto: ($) => seq($.INTO, $.identifier),
    readWith: ($) =>
      seq(optional($.WITH), choice(seq(choice($.KEPT, $.NO), $.LOCK), $.WAIT)),
    readKey: ($) => seq($.KEY, optional($.IS), $.qualifiedDataName),
    // receive statement
    receiveStatement: ($) =>
      seq(
        $.RECEIVE,
        choice($.receiveFromStatement, $.receiveIntoStatement),
        optional($.onExceptionClause),
        optional($.notOnExceptionClause),
        optional($.END_RECEIVE)
      ),
    receiveFromStatement: ($) =>
      seq(
        $.dataName,
        $.FROM,
        $.receiveFrom,
        repeat(
          choice(
            $.receiveBefore,
            $.receiveWith,
            $.receiveThread,
            $.receiveSize,
            $.receiveStatus
          )
        )
      ),
    receiveFrom: ($) =>
      choice(
        seq($.THREAD, $.dataName),
        seq($.LAST, $.THREAD),
        seq($.ANY, $.THREAD)
      ),
    receiveIntoStatement: ($) =>
      seq(
        $.cdName,
        choice($.MESSAGE, $.SEGMENT),
        optional($.INTO),
        $.identifier,
        optional($.receiveNoData),
        optional($.receiveWithData)
      ),
    receiveNoData: ($) => seq($.NO, $.DATA, repeat($.statement)),
    receiveWithData: ($) => seq($.WITH, $.DATA, repeat($.statement)),
    receiveBefore: ($) =>
      seq($.BEFORE, optional($.TIME), choice($.numericLiteral, $.identifier)),
    receiveWith: ($) => seq(optional($.WITH), $.NO, $.WAIT),
    receiveThread: ($) => seq($.THREAD, optional($.IN), $.dataName),
    receiveSize: ($) =>
      seq($.SIZE, optional($.IN), choice($.numericLiteral, $.identifier)),
    receiveStatus: ($) => seq($.STATUS, optional($.IN), $.identifier),
    // release statement
    releaseStatement: ($) =>
      seq($.RELEASE, $.recordName, optional(seq($.FROM, $.qualifiedDataName))),
    // return statement
    returnStatement: ($) =>
      seq(
        $.RETURN,
        $.fileName,
        optional($.RECORD),
        optional($.returnInto),
        $.atEndPhrase,
        optional($.notAtEndPhrase),
        optional($.END_RETURN)
      ),
    returnInto: ($) => seq($.INTO, $.qualifiedDataName),
    // rewrite statement
    rewriteStatement: ($) =>
      seq(
        $.REWRITE,
        $.recordName,
        optional($.rewriteFrom),
        optional($.invalidKeyPhrase),
        optional($.notInvalidKeyPhrase),
        optional($.END_REWRITE)
      ),
    rewriteFrom: ($) => seq($.FROM, $.identifier),
    // search statement
    searchStatement: ($) =>
      seq(
        $.SEARCH,
        optional($.ALL),
        $.qualifiedDataName,
        optional($.searchVarying),
        optional($.atEndPhrase),
        repeat1($.searchWhen),
        optional($.END_SEARCH)
      ),
    searchVarying: ($) => seq($.VARYING, $.qualifiedDataName),
    searchWhen: ($) =>
      seq(
        $.WHEN,
        $.condition,
        choice(seq($.NEXT, $.SENTENCE), repeat($.statement))
      ),
    // send statement
    sendStatement: ($) =>
      seq(
        $.SEND,
        choice($.sendStatementSync, $.sendStatementAsync),
        optional($.onExceptionClause),
        optional($.notOnExceptionClause)
      ),
    sendStatementSync: ($) =>
      seq(
        choice($.identifier, $.literal),
        optional($.sendFromPhrase),
        optional($.sendWithPhrase),
        optional($.sendReplacingPhrase),
        optional($.sendAdvancingPhrase)
      ),
    sendStatementAsync: ($) => seq($.TO, choice($.TOP, $.BOTTOM), $.identifier),
    sendFromPhrase: ($) => seq($.FROM, $.identifier),
    sendWithPhrase: ($) =>
      seq($.WITH, choice($.EGI, $.EMI, $.ESI, $.identifier)),
    sendReplacingPhrase: ($) => seq($.REPLACING, optional($.LINE)),
    sendAdvancingPhrase: ($) =>
      seq(
        choice($.BEFORE, $.AFTER),
        optional($.ADVANCING),
        choice(
          $.sendAdvancingPage,
          $.sendAdvancingLines,
          $.sendAdvancingMnemonic
        )
      ),
    sendAdvancingPage: ($) => $.PAGE,
    sendAdvancingLines: ($) =>
      seq(choice($.identifier, $.literal), optional(choice($.LINE, $.LINES))),
    sendAdvancingMnemonic: ($) => $.mnemonicName,
    // set statement
    setStatement: ($) =>
      seq($.SET, choice(repeat1($.setToStatement), $.setUpDownByStatement)),
    setToStatement: ($) => seq(repeat1($.setTo), $.TO, repeat1($.setToValue)),
    setUpDownByStatement: ($) =>
      seq(
        repeat1($.setTo),
        choice(seq($.UP, $.BY), seq($.DOWN, $.BY)),
        $.setByValue
      ),
    setTo: ($) => $.identifier,
    setToValue: ($) =>
      seq(
        choice(
          $.ON,
          $.OFF,
          seq($.ENTRY, choice($.identifier, $.literal)),
          $.identifier,
          $.literal
        )
      ),
    setByValue: ($) => choice($.identifier, $.literal),
    // sort statement
    sortStatement: ($) =>
      seq(
        $.SORT,
        $.fileName,
        repeat1($.sortOnKeyClause),
        optional($.sortDuplicatesPhrase),
        optional($.sortCollatingSequencePhrase),
        optional($.sortInputProcedurePhrase),
        repeat($.sortUsing),
        optional($.sortOutputProcedurePhrase),
        repeat($.sortGivingPhrase)
      ),
    sortOnKeyClause: ($) =>
      seq(
        optional($.ON),
        choice($.ASCENDING, $.DESCENDING),
        optional($.KEY),
        repeat1($.qualifiedDataName)
      ),
    sortDuplicatesPhrase: ($) =>
      seq(optional($.WITH), $.DUPLICATES, optional($.IN), optional($.ORDER)),
    sortCollatingSequencePhrase: ($) =>
      seq(
        optional($.COLLATING),
        $.SEQUENCE,
        optional($.IS),
        repeat1($.alphabetName),
        optional($.sortCollatingAlphanumeric),
        optional($.sortCollatingNational)
      ),
    sortCollatingAlphanumeric: ($) =>
      seq(optional($.FOR), $.ALPHANUMERIC, $.IS, $.alphabetName),
    sortCollatingNational: ($) =>
      seq(optional($.FOR), $.NATIONAL, optional($.IS), $.alphabetName),
    sortInputProcedurePhrase: ($) =>
      seq(
        $.INPUT,
        $.PROCEDURE,
        optional($.IS),
        $.procedureName,
        optional($.sortInputThrough)
      ),
    sortInputThrough: ($) => seq(choice($.THROUGH, $.THRU), $.procedureName),
    sortUsing: ($) => seq($.USING, repeat1($.fileName)),
    sortOutputProcedurePhrase: ($) =>
      seq(
        $.OUTPUT,
        $.PROCEDURE,
        optional($.IS),
        $.procedureName,
        optional($.sortOutputThrough)
      ),
    sortOutputThrough: ($) => seq(choice($.THROUGH, $.THRU), $.procedureName),
    sortGivingPhrase: ($) => seq($.GIVING, repeat1($.sortGiving)),
    sortGiving: ($) =>
      seq(
        $.fileName,
        optional(
          choice(
            $.LOCK,
            $.SAVE,
            seq($.NO, $.REWIND),
            $.CRUNCH,
            $.RELEASE,
            seq($.WITH, $.REMOVE, $.CRUNCH)
          )
        )
      ),
    // start statement
    startStatement: ($) =>
      seq(
        $.START,
        $.fileName,
        optional($.startKey),
        optional($.invalidKeyPhrase),
        optional($.notInvalidKeyPhrase),
        optional($.END_START)
      ),
    startKey: ($) =>
      seq(
        $.KEY,
        optional($.IS),
        choice(
          seq($.EQUAL, optional($.TO)),
          $.EQUALCHAR,
          seq($.GREATER, optional($.THAN)),
          $.MORETHANCHAR,
          seq($.NOT, $.LESS, optional($.THAN)),
          seq($.NOT, $.LESSTHANCHAR),
          seq($.GREATER, optional($.THAN), $.OR, $.EQUAL, optional($.TO)),
          $.MORETHANOREQUAL
        ),
        $.qualifiedDataName
      ),
    // stop statement
    stopStatement: ($) =>
      seq($.STOP, choice($.RUN, $.literal, $.stopStatementGiving)),
    stopStatementGiving: ($) =>
      seq(
        $.RUN,
        choice($.GIVING, $.RETURNING),
        choice($.identifier, $.integerLiteral)
      ),
    // string statement
    stringStatement: ($) =>
      seq(
        $.STRING,
        repeat1($.stringSendingPhrase),
        $.stringIntoPhrase,
        optional($.stringWithPointerPhrase),
        optional($.onOverflowPhrase),
        optional($.notOnOverflowPhrase),
        optional($.END_STRING)
      ),
    stringSendingPhrase: ($) =>
      seq(
        $.stringSending,
        repeat(seq(optional($.COMMACHAR), $.stringSending)),
        choice($.stringDelimitedByPhrase, $.stringForPhrase)
      ),
    stringSending: ($) => choice($.identifier, $.literal),
    stringDelimitedByPhrase: ($) =>
      seq($.DELIMITED, optional($.BY), choice($.SIZE, $.identifier, $.literal)),
    stringForPhrase: ($) => seq($.FOR, choice($.identifier, $.literal)),
    stringIntoPhrase: ($) => seq($.INTO, $.identifier),
    stringWithPointerPhrase: ($) =>
      seq(optional($.WITH), $.POINTER, $.qualifiedDataName),
    // subtract statement
    subtractStatement: ($) =>
      seq(
        $.SUBTRACT,
        choice(
          $.subtractFromStatement,
          $.subtractFromGivingStatement,
          $.subtractCorrespondingStatement
        ),
        optional($.onSizeErrorPhrase),
        optional($.notOnSizeErrorPhrase),
        optional($.END_SUBTRACT)
      ),
    subtractFromStatement: ($) =>
      seq(repeat1($.subtractSubtrahend), $.FROM, repeat1($.subtractMinuend)),
    subtractFromGivingStatement: ($) =>
      seq(
        repeat1($.subtractSubtrahend),
        $.FROM,
        $.subtractMinuendGiving,
        $.GIVING,
        repeat1($.subtractGiving)
      ),
    subtractCorrespondingStatement: ($) =>
      seq(
        choice($.CORRESPONDING, $.CORR),
        $.qualifiedDataName,
        $.FROM,
        $.subtractMinuendCorresponding
      ),
    subtractSubtrahend: ($) => choice($.identifier, $.literal),
    subtractMinuend: ($) => seq($.identifier, optional($.ROUNDED)),
    subtractMinuendGiving: ($) => choice($.identifier, $.literal),
    subtractGiving: ($) => seq($.identifier, optional($.ROUNDED)),
    subtractMinuendCorresponding: ($) =>
      seq($.qualifiedDataName, optional($.ROUNDED)),
    // terminate statement
    terminateStatement: ($) => seq($.TERMINATE, $.reportName),
    // unstring statement
    unstringStatement: ($) =>
      seq(
        $.UNSTRING,
        $.unstringSendingPhrase,
        $.unstringIntoPhrase,
        optional($.unstringWithPointerPhrase),
        optional($.unstringTallyingPhrase),
        optional($.onOverflowPhrase),
        optional($.notOnOverflowPhrase),
        optional($.END_UNSTRING)
      ),
    unstringSendingPhrase: ($) =>
      seq(
        $.identifier,
        optional(
          seq($.unstringDelimitedByPhrase, repeat($.unstringOrAllPhrase))
        )
      ),
    unstringDelimitedByPhrase: ($) =>
      seq(
        $.DELIMITED,
        optional($.BY),
        optional($.ALL),
        choice($.identifier, $.literal)
      ),
    unstringOrAllPhrase: ($) =>
      seq($.OR, optional($.ALL), choice($.identifier, $.literal)),
    unstringIntoPhrase: ($) => seq($.INTO, repeat1($.unstringInto)),
    unstringInto: ($) =>
      seq(
        $.identifier,
        optional($.unstringDelimiterIn),
        optional($.unstringCountIn)
      ),
    unstringDelimiterIn: ($) => seq($.DELIMITER, optional($.IN), $.identifier),
    unstringCountIn: ($) => seq($.COUNT, optional($.IN), $.identifier),
    unstringWithPointerPhrase: ($) =>
      seq(optional($.WITH), $.POINTER, $.qualifiedDataName),
    unstringTallyingPhrase: ($) =>
      seq($.TALLYING, optional($.IN), $.qualifiedDataName),
    // use statement
    useStatement: ($) => seq($.USE, choice($.useAfterClause, $.useDebugClause)),
    useAfterClause: ($) =>
      seq(
        optional($.GLOBAL),
        $.AFTER,
        optional($.STANDARD),
        choice($.EXCEPTION, $.ERROR),
        $.PROCEDURE,
        optional($.ON),
        $.useAfterOn
      ),
    useAfterOn: ($) =>
      seq(choice($.INPUT, $.OUTPUT, $.I_O, $.EXTEND, repeat1($.fileName))),
    useDebugClause: ($) =>
      seq(optional($.FOR), $.DEBUGGING, optional($.ON), repeat1($.useDebugOn)),
    useDebugOn: ($) =>
      seq(
        $.ALL,
        choice($.PROCEDURES, $.ALL),
        optional($.REFERENCES),
        optional($.OF),
        choice($.identifier, $.procedureName, $.fileName)
      ),
    // write statement
    writeStatement: ($) =>
      seq(
        $.WRITE,
        $.recordName,
        optional($.writeFromPhrase),
        optional($.writeAdvancingPhrase),
        optional($.writeAtEndOfPagePhrase),
        optional($.writeNotAtEndOfPagePhrase),
        optional($.invalidKeyPhrase),
        optional($.notInvalidKeyPhrase),
        optional($.END_WRITE)
      ),
    writeFromPhrase: ($) => seq($.FROM, choice($.identifier, $.literal)),
    writeAdvancingPhrase: ($) =>
      seq(
        choice($.BEFORE, $.AFTER),
        optional($.ADVANCING),
        choice(
          $.writeAdvancingPage,
          $.writeAdvancingLines,
          $.writeAdvancingMnemonic
        )
      ),
    writeAdvancingPage: ($) => $.PAGE,
    writeAdvancingLines: ($) =>
      seq(choice($.identifier, $.literal), optional(choice($.LINE, $.LINES))),
    writeAdvancingMnemonic: ($) => $.mnemonicName,
    writeAtEndOfPagePhrase: ($) =>
      seq(optional($.AT), choice($.END_OF_PAGE, $.EOP), repeat($.statement)),
    writeNotAtEndOfPagePhrase: ($) =>
      seq(
        $.NOT,
        optional($.AT),
        choice($.END_OF_PAGE, $.EOP),
        repeat($.statement)
      ),
    // statement phrases ----------------------------------
    atEndPhrase: ($) => seq(optional($.AT), $.END, repeat($.statement)),
    notAtEndPhrase: ($) =>
      seq($.NOT, optional($.AT), $.END, repeat($.statement)),
    invalidKeyPhrase: ($) =>
      seq($.INVALID, optional($.KEY), repeat($.statement)),
    notInvalidKeyPhrase: ($) =>
      seq($.NOT, $.INVALID, optional($.KEY), repeat($.statement)),
    onOverflowPhrase: ($) =>
      seq(optional($.ON), $.OVERFLOW, repeat($.statement)),
    notOnOverflowPhrase: ($) =>
      seq($.NOT, optional($.ON), $.OVERFLOW, repeat($.statement)),
    onSizeErrorPhrase: ($) =>
      seq(optional($.ON), $.SIZE, $.ERROR, repeat($.statement)),
    notOnSizeErrorPhrase: ($) =>
      seq($.NOT, optional($.ON), $.SIZE, $.ERROR, repeat($.statement)),
    // statement clauses ----------------------------------
    onExceptionClause: ($) =>
      seq(optional($.ON), $.EXCEPTION, repeat($.statement)),
    notOnExceptionClause: ($) =>
      seq($.NOT, optional($.ON), $.EXCEPTION, repeat($.statement)),
    // arithmetic expression ----------------------------------
    arithmeticExpression: ($) => seq($.multDivs, repeat($.plusMinus)),
    plusMinus: ($) => seq(choice($.PLUSCHAR, $._MINUSCHAR), $.multDivs),
    multDivs: ($) => seq($.powers, repeat($.multDiv)),
    multDiv: ($) => seq(choice($.ASTERISKCHAR, $.SLASHCHAR), $.powers),
    powers: ($) =>
      seq(optional(choice($.PLUSCHAR, $._MINUSCHAR)), $.basis, repeat($.power)),
    power: ($) => seq($.DOUBLEASTERISKCHAR, $.basis),
    basis: ($) =>
      seq(
        $.LPARENCHAR,
        $.arithmeticExpression,
        choice($.RPARENCHAR, $.identifier, $.literal)
      ),
    // condition ----------------------------------
    condition: ($) => seq($.combinableCondition, repeat($.andOrCondition)),
    andOrCondition: ($) =>
      seq(
        choice($.AND, $.OR),
        choice($.combinableCondition, repeat1($.abbreviation))
      ),
    combinableCondition: ($) => seq(optional($.NOT), $.simpleCondition),
    simpleCondition: ($) =>
      seq(
        $.LPARENCHAR,
        $.condition,
        choice(
          $.RPARENCHAR,
          $.relationCondition,
          $.classCondition,
          $.conditionNameReference
        )
      ),
    classCondition: ($) =>
      seq(
        $.identifier,
        optional($.IS),
        optional($.NOT),
        choice(
          $.NUMERIC,
          $.ALPHABETIC,
          $.ALPHABETIC_LOWER,
          $.ALPHABETIC_UPPER,
          $.DBCS,
          $.KANJI,
          $.className
        )
      ),
    conditionNameReference: ($) =>
      seq(
        $.conditionName,
        choice(
          seq(
            repeat($.inData),
            optional($.inFile),
            repeat($.conditionNameSubscriptReference)
          ),
          repeat($.inMnemonic)
        )
      ),
    conditionNameSubscriptReference: ($) =>
      seq(
        $.LPARENCHAR,
        $.subscript,
        repeat(seq(optional($.COMMACHAR), $.subscript)),
        $.RPARENCHAR
      ),
    // relation ----------------------------------
    relationCondition: ($) =>
      choice(
        $.relationSignCondition,
        $.relationArithmeticComparison,
        $.relationCombinedComparison
      ),
    relationSignCondition: ($) =>
      seq(
        $.arithmeticExpression,
        optional($.IS),
        optional($.NOT),
        choice($.POSITIVE, $.NEGATIVE, $.ZERO)
      ),
    relationArithmeticComparison: ($) =>
      seq($.arithmeticExpression, $.relationalOperator, $.arithmeticExpression),
    relationCombinedComparison: ($) =>
      seq(
        $.arithmeticExpression,
        $.relationalOperator,
        $.LPARENCHAR,
        $.relationCombinedCondition,
        $.RPARENCHAR
      ),
    relationCombinedCondition: ($) =>
      seq(
        $.arithmeticExpression,
        repeat1(seq(choice($.AND, $.OR), $.arithmeticExpression))
      ),
    relationalOperator: ($) =>
      seq(
        optional(choice($.IS, $.ARE)),
        choice(
          seq(
            optional($.NOT),
            choice(
              seq($.GREATER, optional($.THAN)),
              $.MORETHANCHAR,
              seq($.LESS, optional($.THAN)),
              $.LESSTHANCHAR,
              seq($.EQUAL, optional($.TO)),
              $.EQUALCHAR
            )
          ),
          $.NOTEQUALCHAR,
          seq($.GREATER, optional($.THAN), $.OR, $.EQUAL, optional($.TO)),
          $.MORETHANOREQUAL,
          seq($.LESS, optional($.THAN), $.OR, $.EQUAL, optional($.TO)),
          $.LESSTHANOREQUAL
        )
      ),
    abbreviation: ($) =>
      seq(
        optional($.NOT),
        optional($.relationalOperator),
        choice(
          $.arithmeticExpression,
          seq(
            $.LPARENCHAR,
            $.arithmeticExpression,
            $.abbreviation,
            $.RPARENCHAR
          )
        )
      ),
    // identifier ----------------------------------
    identifier: ($) =>
      choice(
        $.qualifiedDataName,
        $.tableCall,
        $.functionCall,
        $.specialRegister
      ),
    tableCall: ($) =>
      seq(
        $.qualifiedDataName,
        repeat(
          seq(
            $.LPARENCHAR,
            $.subscript,
            repeat(seq(optional($.COMMACHAR), $.subscript)),
            $.RPARENCHAR
          )
        ),
        optional($.referenceModifier)
      ),
    functionCall: ($) =>
      seq(
        $.FUNCTION,
        $.functionName,
        repeat(
          seq(
            $.LPARENCHAR,
            $.argument,
            repeat(seq(optional($.COMMACHAR), $.argument)),
            $.RPARENCHAR
          )
        ),
        optional($.referenceModifier)
      ),
    referenceModifier: ($) =>
      seq(
        $.LPARENCHAR,
        $.characterPosition,
        $.COLONCHAR,
        optional($.length),
        $.RPARENCHAR
      ),
    characterPosition: ($) => $.arithmeticExpression,
    length: ($) => $.arithmeticExpression,
    subscript: ($) =>
      seq(
        choice($.ALL, $.integerLiteral, $.qualifiedDataName),
        choice(optional($.integerLiteral), $.indexName),
        choice(optional($.integerLiteral), $.arithmeticExpression)
      ),
    argument: ($) =>
      seq(
        choice($.literal, $.identifier, $.qualifiedDataName),
        choice(optional($.integerLiteral), $.indexName),
        choice(optional($.integerLiteral), $.arithmeticExpression)
      ),
    // qualified data name ----------------------------------
    qualifiedDataName: ($) =>
      choice(
        $.qualifiedDataNameFormat1,
        $.qualifiedDataNameFormat2,
        $.qualifiedDataNameFormat3,
        $.qualifiedDataNameFormat4
      ),
    qualifiedDataNameFormat1: ($) =>
      seq(
        choice($.dataName, $.conditionName),
        optional(
          choice(seq(repeat1($.qualifiedInData), optional($.inFile)), $.inFile)
        )
      ),
    qualifiedDataNameFormat2: ($) => seq($.paragraphName, $.inSection),
    qualifiedDataNameFormat3: ($) => seq($.textName, $.inLibrary),
    qualifiedDataNameFormat4: ($) => seq($.LINAGE_COUNTER, $.inFile),
    qualifiedInData: ($) => choice($.inData, $.inTable),
    // in ----------------------------------
    inData: ($) => seq(choice($.IN, $.OF), $.dataName),
    inFile: ($) => seq(choice($.IN, $.OF), $.fileName),
    inMnemonic: ($) => seq(choice($.IN, $.OF), $.mnemonicName),
    inSection: ($) => seq(choice($.IN, $.OF), $.sectionName),
    inLibrary: ($) => seq(choice($.IN, $.OF), $.libraryName),
    inTable: ($) => seq(choice($.IN, $.OF), $.tableCall),
    // names ----------------------------------
    alphabetName: ($) => $.cobolWord,
    assignmentName: ($) => $.systemName,
    basisName: ($) => $.programName,
    cdName: ($) => $.cobolWord,
    className: ($) => $.cobolWord,
    computerName: ($) => $.systemName,
    conditionName: ($) => $.cobolWord,
    dataName: ($) => $.cobolWord,
    dataDescName: ($) => choice($.FILLER, $.CURSOR, $.dataName),
    environmentName: ($) => $.systemName,
    fileName: ($) => $.cobolWord,
    functionName: ($) =>
      prec(
        2,
        choice(
          $.INTEGER,
          $.LENGTH,
          $.RANDOM,
          $.SUM,
          $.WHEN_COMPILED,
          $.cobolWord
        )
      ),
    indexName: ($) => $.cobolWord,
    // languageName: ($) => $.systemName,
    libraryName: ($) => $.cobolWord,
    localName: ($) => $.cobolWord,
    mnemonicName: ($) => $.cobolWord,
    paragraphName: ($) => choice($.cobolWord, $.integerLiteral),
    procedureName: ($) =>
      seq($.paragraphName, choice(optional($.inSection), $.sectionName)),
    programName: ($) => choice($.NONNUMERICLITERAL, $.cobolWord),
    recordName: ($) => $.qualifiedDataName,
    reportName: ($) => $.qualifiedDataName,
    // routineName: ($) => $.cobolWord,
    screenName: ($) => $.cobolWord,
    sectionName: ($) => choice($.cobolWord, $.integerLiteral),
    systemName: ($) => $.cobolWord,
    symbolicCharacter: ($) => $.cobolWord,
    textName: ($) => $.cobolWord,
    // literal ----------------------------------
    cobolWord: ($) =>
      choice(
        $.IDENTIFIER,
        $.ABORT,
        $.AS,
        $.ASCII,
        $.ASSOCIATED_DATA,
        $.ASSOCIATED_DATA_LENGTH,
        $.ATTRIBUTE,
        $.AUTO,
        $.AUTO_SKIP,
        $.BACKGROUND_COLOR,
        $.BACKGROUND_COLOUR,
        $.BEEP,
        $.BELL,
        $.BINARY,
        $.BIT,
        $.BLINK,
        $.BLOB,
        $.BOUNDS,
        $.CAPABLE,
        $.CCSVERSION,
        $.CHANGED,
        $.CHANNEL,
        $.CLOB,
        $.CLOSE_DISPOSITION,
        $.COBOL,
        $.COMMITMENT,
        $.CONTROL_POINT,
        $.CONVENTION,
        $.CRUNCH,
        $.CURSOR,
        $.DBCLOB,
        $.DEFAULT,
        $.DEFAULT_DISPLAY,
        $.DEFINITION,
        $.DFHRESP,
        $.DFHVALUE,
        $.DISK,
        $.DONTCARE,
        $.DOUBLE,
        $.EBCDIC,
        $.EMPTY_CHECK,
        $.ENTER,
        $.ENTRY_PROCEDURE,
        $.EOL,
        $.EOS,
        $.ERASE,
        $.ESCAPE,
        $.EVENT,
        $.EXCLUSIVE,
        $.EXPORT,
        $.EXTENDED,
        $.FOREGROUND_COLOR,
        $.FOREGROUND_COLOUR,
        $.FULL,
        $.FUNCTIONNAME,
        $.FUNCTION_POINTER,
        $.GRID,
        $.HIGHLIGHT,
        $.IMPLICIT,
        $.IMPORT,
        $.INTEGER,
        $.KEPT,
        $.KEYBOARD,
        $.LANGUAGE,
        $.LB,
        $.LD,
        $.LEFTLINE,
        $.LENGTH_CHECK,
        $.LIBACCESS,
        $.LIBPARAMETER,
        $.LIBRARY,
        $.LIST,
        $.LOCAL,
        $.LONG_DATE,
        $.LONG_TIME,
        $.LOWER,
        $.LOWLIGHT,
        $.MMDDYYYY,
        $.NAMED,
        $.NATIONAL,
        $.NATIONAL_EDITED,
        $.NETWORK,
        $.NO_ECHO,
        $.NUMERIC_DATE,
        $.NUMERIC_TIME,
        $.ODT,
        $.ORDERLY,
        $.OVERLINE,
        $.OWN,
        $.PASSWORD,
        $.PORT,
        $.PRINTER,
        $.PRIVATE,
        $.PROCESS,
        $.PROGRAM,
        $.PROMPT,
        $.READER,
        $.REAL,
        $.RECEIVED,
        $.RECURSIVE,
        $.REF,
        $.REMOTE,
        $.REMOVE,
        $.REQUIRED,
        $.REVERSE_VIDEO,
        $.SAVE,
        $.SECURE,
        $.SHARED,
        $.SHAREDBYALL,
        $.SHAREDBYRUNUNIT,
        $.SHARING,
        $.SHORT_DATE,
        $.SQL,
        $.SYMBOL,
        $.TASK,
        $.THREAD,
        $.THREAD_LOCAL,
        $.TIMER,
        $.TODAYS_DATE,
        $.TODAYS_NAME,
        $.TRUNCATED,
        $.TYPEDEF,
        $.UNDERLINE,
        $.VIRTUAL,
        $.WAIT,
        $.YEAR,
        $.YYYYMMDD,
        $.YYYYDDD,
        $.ZERO_FILL
      ),
    literal: ($) =>
      choice(
        $.NONNUMERICLITERAL,
        $.figurativeConstant,
        $.numericLiteral,
        $.booleanLiteral,
        $.cicsDfhRespLiteral,
        $.cicsDfhValueLiteral
      ),
    booleanLiteral: ($) => choice($.TRUE, $.FALSE),
    numericLiteral: ($) => choice($.NUMERICLITERAL, $.ZERO, $.integerLiteral),
    integerLiteral: ($) =>
      choice(
        $.INTEGERLITERAL,
        $.LEVEL_NUMBER_66,
        $.LEVEL_NUMBER_77,
        $.LEVEL_NUMBER_88
      ),
    cicsDfhRespLiteral: ($) =>
      prec(
        2,
        seq(
          $.DFHRESP,
          $.LPARENCHAR,
          choice($.cobolWord, $.literal),
          $.RPARENCHAR
        )
      ),
    cicsDfhValueLiteral: ($) =>
      prec(
        2,
        seq(
          $.DFHVALUE,
          $.LPARENCHAR,
          choice($.cobolWord, $.literal),
          $.RPARENCHAR
        )
      ),
    // keywords ----------------------------------
    figurativeConstant: ($) =>
      seq(
        $.ALL,
        choice(
          $.literal,
          $.HIGH_VALUE,
          $.HIGH_VALUES,
          $.LOW_VALUE,
          $.LOW_VALUES,
          $.NULL,
          $.NULLS,
          $.QUOTE,
          $.QUOTES,
          $.SPACE,
          $.SPACES,
          $.ZERO,
          $.ZEROS,
          $.ZEROES
        )
      ),
    specialRegister: ($) =>
      seq(
        $.ADDRESS,
        $.OF,
        choice(
          $.identifier,
          $.DATE,
          $.DAY,
          $.DAY_OF_WEEK,
          $.DEBUG_CONTENTS,
          $.DEBUG_ITEM,
          $.DEBUG_LINE,
          $.DEBUG_NAME,
          $.DEBUG_SUB_1,
          $.DEBUG_SUB_2,
          $.DEBUG_SUB_3,
          $.LENGTH
        ),
        optional($.OF),
        choice(
          $.identifier,
          $.LINAGE_COUNTER,
          $.LINE_COUNTER,
          $.PAGE_COUNTER,
          $.RETURN_CODE,
          $.SHIFT_IN,
          $.SHIFT_OUT,
          $.SORT_CONTROL,
          $.SORT_CORE_SIZE,
          $.SORT_FILE_SIZE,
          $.SORT_MESSAGE,
          $.SORT_MODE_SIZE,
          $.SORT_RETURN,
          $.TALLY,
          $.TIME,
          $.WHEN_COMPILED
        )
      ),
    // comment entry
    // TODO: multiple indented comment entries are allowed
    commentEntry: ($) => /[^\n\r]*/,
    // lexer rules --------------------------------------------------------------------------------
    // keywords
    ABORT: ($) => seq($._A, $._B, $._O, $._R, $._T),
    ACCEPT: ($) => seq($._A, $._C, $._C, $._E, $._P, $._T),
    ACCESS: ($) => seq($._A, $._C, $._C, $._E, $._S, $._S),
    ADD: ($) => seq($._A, $._D, $._D),
    ADDRESS: ($) => prec(2, seq($._A, $._D, $._D, $._R, $._E, $._S, $._S)),
    ADVANCING: ($) => seq($._A, $._D, $._V, $._A, $._N, $._C, $._I, $._N, $._G),
    AFTER: ($) => seq($._A, $._F, $._T, $._E, $._R),
    ALIGNED: ($) => seq($._A, $._L, $._I, $._G, $._N, $._E, $._D),
    ALL: ($) => seq($._A, $._L, $._L),
    ALPHABET: ($) => seq($._A, $._L, $._P, $._H, $._A, $._B, $._E, $._T),
    ALPHABETIC: ($) =>
      prec(2, seq($._A, $._L, $._P, $._H, $._A, $._B, $._E, $._T, $._I, $._C)),
    ALPHABETIC_LOWER: ($) =>
      prec(
        3,
        seq(
          $._A,
          $._L,
          $._P,
          $._H,
          $._A,
          $._B,
          $._E,
          $._T,
          $._I,
          $._C,
          $._MINUSCHAR,
          $._L,
          $._O,
          $._W,
          $._E,
          $._R
        )
      ),
    ALPHABETIC_UPPER: ($) =>
      prec(
        3,
        seq(
          $._A,
          $._L,
          $._P,
          $._H,
          $._A,
          $._B,
          $._E,
          $._T,
          $._I,
          $._C,
          $._MINUSCHAR,
          $._U,
          $._P,
          $._P,
          $._E,
          $._R
        )
      ),
    ALPHANUMERIC: ($) =>
      seq(
        $._A,
        $._L,
        $._P,
        $._H,
        $._A,
        $._N,
        $._U,
        $._M,
        $._E,
        $._R,
        $._I,
        $._C
      ),
    ALPHANUMERIC_EDITED: ($) =>
      prec(
        2,
        seq(
          $._A,
          $._L,
          $._P,
          $._H,
          $._A,
          $._N,
          $._U,
          $._M,
          $._E,
          $._R,
          $._I,
          $._C,
          $._MINUSCHAR,
          $._E,
          $._D,
          $._I,
          $._T,
          $._E,
          $._D
        )
      ),
    ALSO: ($) => seq($._A, $._L, $._S, $._O),
    ALTER: ($) => seq($._A, $._L, $._T, $._E, $._R),
    ALTERNATE: ($) =>
      prec(2, seq($._A, $._L, $._T, $._E, $._R, $._N, $._A, $._T, $._E)),
    AND: ($) => seq($._A, $._N, $._D),
    ANY: ($) => seq($._A, $._N, $._Y),
    ARE: ($) => seq($._A, $._R, $._E),
    AREA: ($) => prec(2, seq($._A, $._R, $._E, $._A)),
    AREAS: ($) => prec(3, seq($._A, $._R, $._E, $._A, $._S)),
    AS: ($) => seq($._A, $._S),
    ASCENDING: ($) =>
      prec(2, seq($._A, $._S, $._C, $._E, $._N, $._D, $._I, $._N, $._G)),
    ASCII: ($) => prec(2, seq($._A, $._S, $._C, $._I, $._I)),
    ASSIGN: ($) => prec(2, seq($._A, $._S, $._S, $._I, $._G, $._N)),
    ASSOCIATED_DATA: ($) =>
      prec(
        2,
        seq(
          $._A,
          $._S,
          $._S,
          $._O,
          $._C,
          $._I,
          $._A,
          $._T,
          $._E,
          $._D,
          $._MINUSCHAR,
          $._D,
          $._A,
          $._T,
          $._A
        )
      ),
    ASSOCIATED_DATA_LENGTH: ($) =>
      prec(
        3,
        seq(
          $._A,
          $._S,
          $._S,
          $._O,
          $._C,
          $._I,
          $._A,
          $._T,
          $._E,
          $._D,
          $._MINUSCHAR,
          $._D,
          $._A,
          $._T,
          $._A,
          $._MINUSCHAR,
          $._L,
          $._E,
          $._N,
          $._G,
          $._T,
          $._H
        )
      ),
    AT: ($) => seq($._A, $._T),
    ATTRIBUTE: ($) =>
      prec(2, seq($._A, $._T, $._T, $._R, $._I, $._B, $._U, $._T, $._E)),
    AUTHOR: ($) => seq($._A, $._U, $._T, $._H, $._O, $._R),
    AUTO: ($) => seq($._A, $._U, $._T, $._O),
    AUTO_SKIP: ($) =>
      prec(
        2,
        seq($._A, $._U, $._T, $._O, $._MINUSCHAR, $._S, $._K, $._I, $._P)
      ),
    BACKGROUND_COLOR: ($) =>
      seq(
        $._B,
        $._A,
        $._C,
        $._K,
        $._G,
        $._R,
        $._O,
        $._U,
        $._N,
        $._D,
        $._MINUSCHAR,
        $._C,
        $._O,
        $._L,
        $._O,
        $._R
      ),
    BACKGROUND_COLOUR: ($) =>
      seq(
        $._B,
        $._A,
        $._C,
        $._K,
        $._G,
        $._R,
        $._O,
        $._U,
        $._N,
        $._D,
        $._MINUSCHAR,
        $._C,
        $._O,
        $._L,
        $._O,
        $._U,
        $._R
      ),
    //BASIS: ($) => seq($._B, $._A, $._S, $._I, $._S),
    BEEP: ($) => seq($._B, $._E, $._E, $._P),
    BEFORE: ($) => seq($._B, $._E, $._F, $._O, $._R, $._E),
    //BEGINNING: ($) => seq($._B, $._E, $._G, $._I, $._N, $._N, $._I, $._N, $._G),
    BELL: ($) => seq($._B, $._E, $._L, $._L),
    BINARY: ($) => seq($._B, $._I, $._N, $._A, $._R, $._Y),
    BIT: ($) => seq($._B, $._I, $._T),
    BLANK: ($) => seq($._B, $._L, $._A, $._N, $._K),
    BLINK: ($) => seq($._B, $._L, $._I, $._N, $._K),
    BLOB: ($) => seq($._B, $._L, $._O, $._B),
    BLOCK: ($) => seq($._B, $._L, $._O, $._C, $._K),
    BOUNDS: ($) => seq($._B, $._O, $._U, $._N, $._D, $._S),
    BOTTOM: ($) => seq($._B, $._O, $._T, $._T, $._O, $._M),
    BY: ($) => seq($._B, $._Y),
    BYFUNCTION: ($) =>
      seq($._B, $._Y, $._F, $._U, $._N, $._C, $._T, $._I, $._O, $._N),
    BYTITLE: ($) => seq($._B, $._Y, $._T, $._I, $._T, $._L, $._E),
    CALL: ($) => seq($._C, $._A, $._L, $._L),
    CANCEL: ($) => seq($._C, $._A, $._N, $._C, $._E, $._L),
    CAPABLE: ($) => seq($._C, $._A, $._P, $._A, $._B, $._L, $._E),
    CCSVERSION: ($) =>
      seq($._C, $._C, $._S, $._V, $._E, $._R, $._S, $._I, $._O, $._N),
    CD: ($) => seq($._C, $._D),
    CF: ($) => seq($._C, $._F),
    CH: ($) => seq($._C, $._H),
    CHAINING: ($) => seq($._C, $._H, $._A, $._I, $._N, $._I, $._N, $._G),
    CHANGED: ($) => seq($._C, $._H, $._A, $._N, $._G, $._E, $._D),
    CHANNEL: ($) => seq($._C, $._H, $._A, $._N, $._N, $._E, $._L),
    CHARACTER: ($) => seq($._C, $._H, $._A, $._R, $._A, $._C, $._T, $._E, $._R),
    CHARACTERS: ($) =>
      prec(2, seq($._C, $._H, $._A, $._R, $._A, $._C, $._T, $._E, $._R, $._S)),
    CLASS: ($) => seq($._C, $._L, $._A, $._S, $._S),
    CLASS_ID: ($) =>
      prec(2, seq($._C, $._L, $._A, $._S, $._S, $._MINUSCHAR, $._I, $._D)),
    CLOB: ($) => seq($._C, $._L, $._O, $._B),
    CLOCK_UNITS: ($) =>
      seq(
        $._C,
        $._L,
        $._O,
        $._C,
        $._K,
        $._MINUSCHAR,
        $._U,
        $._N,
        $._I,
        $._T,
        $._S
      ),
    CLOSE: ($) => seq($._C, $._L, $._O, $._S, $._E),
    CLOSE_DISPOSITION: ($) =>
      prec(
        2,
        seq(
          $.CLOSE,
          $._MINUSCHAR,
          $._D,
          $._I,
          $._S,
          $._P,
          $._O,
          $._S,
          $._I,
          $._T,
          $._I,
          $._O,
          $._N
        )
      ),
    COBOL: ($) => seq($._C, $._O, $._B, $._O, $._L),
    CODE: ($) => seq($._C, $._O, $._D, $._E),
    CODE_SET: ($) =>
      prec(2, seq($._C, $._O, $._D, $._E, $._MINUSCHAR, $._S, $._E, $._T)),
    COLLATING: ($) =>
      prec(2, seq($._C, $._O, $._L, $._L, $._A, $._T, $._I, $._N, $._G)),
    COL: ($) => seq($._C, $._O, $._L),
    COLUMN: ($) => prec(2, seq($._C, $._O, $._L, $._U, $._M, $._N)),
    //COM_REG: ($) => seq($._C, $._O, $._M, $.MINUSCHAR, $._R, $._E, $._G),
    COMMA: ($) => seq($._C, $._O, $._M, $._M, $._A),
    COMMITMENT: ($) =>
      seq($._C, $._O, $._M, $._M, $._I, $._T, $._M, $._E, $._N, $._T),
    COMMON: ($) => seq($._C, $._O, $._M, $._M, $._O, $._N),
    COMMUNICATION: ($) =>
      seq(
        $._C,
        $._O,
        $._M,
        $._M,
        $._U,
        $._N,
        $._I,
        $._C,
        $._A,
        $._T,
        $._I,
        $._O,
        $._N
      ),
    COMP: ($) => seq($._C, $._O, $._M, $._P),
    COMP_1: ($) => prec(2, seq($._C, $._O, $._M, $._P, $._MINUSCHAR, "1")),
    COMP_2: ($) => prec(2, seq($._C, $._O, $._M, $._P, $._MINUSCHAR, "2")),
    COMP_3: ($) => prec(2, seq($._C, $._O, $._M, $._P, $._MINUSCHAR, "3")),
    COMP_4: ($) => prec(2, seq($._C, $._O, $._M, $._P, $._MINUSCHAR, "4")),
    COMP_5: ($) => prec(2, seq($._C, $._O, $._M, $._P, $._MINUSCHAR, "5")),
    COMPUTATIONAL: ($) =>
      seq(
        $._C,
        $._O,
        $._M,
        $._P,
        $._U,
        $._T,
        $._A,
        $._T,
        $._I,
        $._O,
        $._N,
        $._A,
        $._L
      ),
    COMPUTATIONAL_1: ($) =>
      prec(
        2,
        seq(
          $._C,
          $._O,
          $._M,
          $._P,
          $._U,
          $._T,
          $._A,
          $._T,
          $._I,
          $._O,
          $._N,
          $._A,
          $._L,
          $._MINUSCHAR,
          "1"
        )
      ),
    COMPUTATIONAL_2: ($) =>
      prec(
        2,
        seq(
          $._C,
          $._O,
          $._M,
          $._P,
          $._U,
          $._T,
          $._A,
          $._T,
          $._I,
          $._O,
          $._N,
          $._A,
          $._L,
          $._MINUSCHAR,
          "2"
        )
      ),
    COMPUTATIONAL_3: ($) =>
      prec(
        2,
        seq(
          $._C,
          $._O,
          $._M,
          $._P,
          $._U,
          $._T,
          $._A,
          $._T,
          $._I,
          $._O,
          $._N,
          $._A,
          $._L,
          $._MINUSCHAR,
          "3"
        )
      ),
    COMPUTATIONAL_4: ($) =>
      prec(
        2,
        seq(
          $._C,
          $._O,
          $._M,
          $._P,
          $._U,
          $._T,
          $._A,
          $._T,
          $._I,
          $._O,
          $._N,
          $._A,
          $._L,
          $._MINUSCHAR,
          "4"
        )
      ),
    COMPUTATIONAL_5: ($) =>
      prec(
        2,
        seq(
          $._C,
          $._O,
          $._M,
          $._P,
          $._U,
          $._T,
          $._A,
          $._T,
          $._I,
          $._O,
          $._N,
          $._A,
          $._L,
          $._MINUSCHAR,
          "5"
        )
      ),
    COMPUTE: ($) => seq($._C, $._O, $._M, $._P, $._U, $._T, $._E),
    CONFIGURATION: ($) =>
      seq(
        $._C,
        $._O,
        $._N,
        $._F,
        $._I,
        $._G,
        $._U,
        $._R,
        $._A,
        $._T,
        $._I,
        $._O,
        $._N
      ),
    CONTAINS: ($) => seq($._C, $._O, $._N, $._T, $._A, $._I, $._N, $._S),
    CONTENT: ($) => seq($._C, $._O, $._N, $._T, $._E, $._N, $._T),
    CONTINUE: ($) => seq($._C, $._O, $._N, $._T, $._I, $._N, $._U, $._E),
    CONTROL: ($) => seq($._C, $._O, $._N, $._T, $._R, $._O, $._L),
    CONTROL_POINT: ($) =>
      prec(
        2,
        seq(
          $._C,
          $._O,
          $._N,
          $._T,
          $._R,
          $._O,
          $._L,
          $._MINUSCHAR,
          $._P,
          $._O,
          $._I,
          $._N,
          $._T
        )
      ),
    CONTROLS: ($) =>
      prec(2, seq($._C, $._O, $._N, $._T, $._R, $._O, $._L, $._S)),
    CONVENTION: ($) =>
      seq($._C, $._O, $._N, $._V, $._E, $._N, $._T, $._I, $._O, $._N),
    CONVERTING: ($) =>
      seq($._C, $._O, $._N, $._V, $._E, $._R, $._T, $._I, $._N, $._G),
    //COPY: ($) => seq($._C, $._O, $._P, $._Y),
    CORR: ($) => seq($._C, $._O, $._R, $._R),
    CORRESPONDING: ($) =>
      prec(
        2,
        seq(
          $._C,
          $._O,
          $._R,
          $._R,
          $._E,
          $._S,
          $._P,
          $._O,
          $._N,
          $._D,
          $._I,
          $._N,
          $._G
        )
      ),
    COUNT: ($) => seq($._C, $._O, $._U, $._N, $._T),
    CRUNCH: ($) => seq($._C, $._R, $._U, $._N, $._C, $._H),
    CURRENCY: ($) => seq($._C, $._U, $._R, $._R, $._E, $._N, $._C, $._Y),
    CURSOR: ($) => seq($._C, $._U, $._R, $._S, $._O, $._R),
    DATA: ($) => seq($._D, $._A, $._T, $._A),
    DATA_BASE: ($) =>
      prec(
        2,
        seq($._D, $._A, $._T, $._A, $._MINUSCHAR, $._B, $._A, $._S, $._E)
      ),
    DATE: ($) => seq($._D, $._A, $._T, $._E),
    DATE_COMPILED: ($) =>
      prec(
        2,
        seq(
          $._D,
          $._A,
          $._T,
          $._E,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._M,
          $._P,
          $._I,
          $._L,
          $._E,
          $._D
        )
      ),
    DATE_WRITTEN: ($) =>
      prec(
        2,
        seq(
          $._D,
          $._A,
          $._T,
          $._E,
          $._MINUSCHAR,
          $._W,
          $._R,
          $._I,
          $._T,
          $._T,
          $._E,
          $._N
        )
      ),
    DAY: ($) => seq($._D, $._A, $._Y),
    DAY_OF_WEEK: ($) =>
      prec(
        2,
        seq(
          $._D,
          $._A,
          $._Y,
          $._MINUSCHAR,
          $._O,
          $._F,
          $._MINUSCHAR,
          $._W,
          $._E,
          $._E,
          $._K
        )
      ),
    DBCS: ($) => seq($._D, $._B, $._C, $._S),
    DBCLOB: ($) => seq($._D, $._B, $._C, $._L, $._O, $._B),
    DE: ($) => seq($._D, $._E),
    DEBUG_CONTENTS: ($) =>
      prec(
        2,
        seq(
          $._D,
          $._E,
          $._B,
          $._U,
          $._G,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._N,
          $._T,
          $._E,
          $._N,
          $._T,
          $._S
        )
      ),
    DEBUG_ITEM: ($) =>
      prec(
        2,
        seq($._D, $._E, $._B, $._U, $._G, $._MINUSCHAR, $._I, $._T, $._E, $._M)
      ),
    DEBUG_LINE: ($) =>
      prec(
        2,
        seq($._D, $._E, $._B, $._U, $._G, $._MINUSCHAR, $._L, $._I, $._N, $._E)
      ),
    DEBUG_NAME: ($) =>
      prec(
        2,
        seq($._D, $._E, $._B, $._U, $._G, $._MINUSCHAR, $._N, $._A, $._M, $._E)
      ),
    DEBUG_SUB_1: ($) =>
      prec(
        2,
        seq(
          $._D,
          $._E,
          $._B,
          $._U,
          $._G,
          $._MINUSCHAR,
          $._S,
          $._U,
          $._B,
          $._MINUSCHAR,
          "1"
        )
      ),
    DEBUG_SUB_2: ($) =>
      prec(
        2,
        seq(
          $._D,
          $._E,
          $._B,
          $._U,
          $._G,
          $._MINUSCHAR,
          $._S,
          $._U,
          $._B,
          $._MINUSCHAR,
          "2"
        )
      ),
    DEBUG_SUB_3: ($) =>
      prec(
        2,
        seq(
          $._D,
          $._E,
          $._B,
          $._U,
          $._G,
          $._MINUSCHAR,
          $._S,
          $._U,
          $._B,
          $._MINUSCHAR,
          "3"
        )
      ),
    DEBUGGING: ($) =>
      prec(2, seq($._D, $._E, $._B, $._U, $._G, $._G, $._I, $._N, $._G)),
    DECIMAL_POINT: ($) =>
      prec(
        2,
        seq(
          $._D,
          $._E,
          $._C,
          $._I,
          $._M,
          $._A,
          $._L,
          $._MINUSCHAR,
          $._P,
          $._O,
          $._I,
          $._N,
          $._T
        )
      ),
    DECLARATIVES: ($) =>
      seq(
        $._D,
        $._E,
        $._C,
        $._L,
        $._A,
        $._R,
        $._A,
        $._T,
        $._I,
        $._V,
        $._E,
        $._S
      ),
    DEFAULT: ($) => seq($._D, $._E, $._F, $._A, $._U, $._L, $._T),
    DEFAULT_DISPLAY: ($) =>
      prec(
        2,
        seq(
          $._D,
          $._E,
          $._F,
          $._A,
          $._U,
          $._L,
          $._T,
          $._MINUSCHAR,
          $._D,
          $._I,
          $._S,
          $._P,
          $._L,
          $._A,
          $._Y
        )
      ),
    DEFINITION: ($) =>
      seq($._D, $._E, $._F, $._I, $._N, $._I, $._T, $._I, $._O, $._N),
    DELETE: ($) => seq($._D, $._E, $._L, $._E, $._T, $._E),
    DELIMITED: ($) => seq($._D, $._E, $._L, $._I, $._M, $._I, $._T, $._E, $._D),
    DELIMITER: ($) => seq($._D, $._E, $._L, $._I, $._M, $._I, $._T, $._E, $._R),
    DEPENDING: ($) => seq($._D, $._E, $._P, $._E, $._N, $._D, $._I, $._N, $._G),
    DESCENDING: ($) =>
      seq($._D, $._E, $._S, $._C, $._E, $._N, $._D, $._I, $._N, $._G),
    DESTINATION: ($) =>
      seq($._D, $._E, $._S, $._T, $._I, $._N, $._A, $._T, $._I, $._O, $._N),
    DETAIL: ($) => seq($._D, $._E, $._T, $._A, $._I, $._L),
    DFHRESP: ($) => seq($._D, $._F, $._H, $._R, $._E, $._S, $._P),
    DFHVALUE: ($) => seq($._D, $._F, $._H, $._V, $._A, $._L, $._U, $._E),
    DISABLE: ($) => seq($._D, $._I, $._S, $._A, $._B, $._L, $._E),
    DISK: ($) => seq($._D, $._I, $._S, $._K),
    DISPLAY: ($) => seq($._D, $._I, $._S, $._P, $._L, $._A, $._Y),
    DISPLAY_1: ($) =>
      prec(2, seq($._D, $._I, $._S, $._P, $._L, $._A, $._Y, $._MINUSCHAR, "1")),
    DIVIDE: ($) => seq($._D, $._I, $._V, $._I, $._D, $._E),
    DIVISION: ($) => seq($._D, $._I, $._V, $._I, $._S, $._I, $._O, $._N),
    DONTCARE: ($) => seq($._D, $._O, $._N, $._T, $._C, $._A, $._R, $._E),
    DOUBLE: ($) => seq($._D, $._O, $._U, $._B, $._L, $._E),
    DOWN: ($) => seq($._D, $._O, $._W, $._N),
    DUPLICATES: ($) =>
      seq($._D, $._U, $._P, $._L, $._I, $._C, $._A, $._T, $._E, $._S),
    DYNAMIC: ($) => seq($._D, $._Y, $._N, $._A, $._M, $._I, $._C),
    EBCDIC: ($) => seq($._E, $._B, $._C, $._D, $._I, $._C),
    EGCS: ($) => seq($._E, $._G, $._C, $._S), // $._E, $._X, $._T, $._E, $._N, $._S, $._I, $._O, N
    EGI: ($) => seq($._E, $._G, $._I),
    ELSE: ($) => seq($._E, $._L, $._S, $._E),
    EMI: ($) => seq($._E, $._M, $._I),
    EMPTY_CHECK: ($) =>
      seq(
        $._E,
        $._M,
        $._P,
        $._T,
        $._Y,
        $._MINUSCHAR,
        $._C,
        $._H,
        $._E,
        $._C,
        $._K
      ),
    ENABLE: ($) => seq($._E, $._N, $._A, $._B, $._L, $._E),
    END: ($) => seq($._E, $._N, $._D),
    END_ACCEPT: ($) =>
      prec(
        2,
        seq($._E, $._N, $._D, $._MINUSCHAR, $._A, $._C, $._C, $._E, $._P, $._T)
      ),
    END_ADD: ($) =>
      prec(2, seq($._E, $._N, $._D, $._MINUSCHAR, $._A, $._D, $._D)),
    END_CALL: ($) =>
      prec(2, seq($._E, $._N, $._D, $._MINUSCHAR, $._C, $._A, $._L, $._L)),
    END_COMPUTE: ($) =>
      prec(
        2,
        seq(
          $._E,
          $._N,
          $._D,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._M,
          $._P,
          $._U,
          $._T,
          $._E
        )
      ),
    END_DELETE: ($) =>
      prec(
        2,
        seq($._E, $._N, $._D, $._MINUSCHAR, $._D, $._E, $._L, $._E, $._T, $._E)
      ),
    END_DISPLAY: ($) =>
      prec(
        2,
        seq(
          $._E,
          $._N,
          $._D,
          $._MINUSCHAR,
          $._D,
          $._I,
          $._S,
          $._P,
          $._L,
          $._A,
          $._Y
        )
      ),
    END_DIVIDE: ($) =>
      prec(
        2,
        seq($._E, $._N, $._D, $._MINUSCHAR, $._D, $._I, $._V, $._I, $._D, $._E)
      ),
    END_EVALUATE: ($) =>
      prec(
        2,
        seq(
          $._E,
          $._N,
          $._D,
          $._MINUSCHAR,
          $._E,
          $._V,
          $._A,
          $._L,
          $._U,
          $._A,
          $._T,
          $._E
        )
      ),
    END_IF: ($) => prec(2, seq($._E, $._N, $._D, $._MINUSCHAR, $._I, $._F)),
    END_MULTIPLY: ($) =>
      prec(
        2,
        seq(
          $._E,
          $._N,
          $._D,
          $._MINUSCHAR,
          $._M,
          $._U,
          $._L,
          $._T,
          $._I,
          $._P,
          $._L,
          $._Y
        )
      ),
    END_OF_PAGE: ($) =>
      prec(
        2,
        seq(
          $._E,
          $._N,
          $._D,
          $._MINUSCHAR,
          $._O,
          $._F,
          $._MINUSCHAR,
          $._P,
          $._A,
          $._G,
          $._E
        )
      ),
    END_PERFORM: ($) =>
      prec(
        2,
        seq(
          $._E,
          $._N,
          $._D,
          $._MINUSCHAR,
          $._P,
          $._E,
          $._R,
          $._F,
          $._O,
          $._R,
          $._M
        )
      ),
    END_READ: ($) =>
      prec(2, seq($._E, $._N, $._D, $._MINUSCHAR, $._R, $._E, $._A, $._D)),
    END_RECEIVE: ($) =>
      prec(
        2,
        seq(
          $._E,
          $._N,
          $._D,
          $._MINUSCHAR,
          $._R,
          $._E,
          $._C,
          $._E,
          $._I,
          $._V,
          $._E
        )
      ),
    END_REMARKS: ($) =>
      prec(
        2,
        seq(
          $._E,
          $._N,
          $._D,
          $._MINUSCHAR,
          $._R,
          $._E,
          $._M,
          $._A,
          $._R,
          $._K,
          $._S
        )
      ),
    END_RETURN: ($) =>
      seq($._E, $._N, $._D, $._MINUSCHAR, $._R, $._E, $._T, $._U, $._R, $._N),
    END_REWRITE: ($) =>
      seq(
        $._E,
        $._N,
        $._D,
        $._MINUSCHAR,
        $._R,
        $._E,
        $._W,
        $._R,
        $._I,
        $._T,
        $._E
      ),
    END_SEARCH: ($) =>
      seq($._E, $._N, $._D, $._MINUSCHAR, $._S, $._E, $._A, $._R, $._C, $._H),
    END_START: ($) =>
      seq($._E, $._N, $._D, $._MINUSCHAR, $._S, $._T, $._A, $._R, $._T),
    END_STRING: ($) =>
      seq($._E, $._N, $._D, $._MINUSCHAR, $._S, $._T, $._R, $._I, $._N, $._G),
    END_SUBTRACT: ($) =>
      seq(
        $._E,
        $._N,
        $._D,
        $._MINUSCHAR,
        $._S,
        $._U,
        $._B,
        $._T,
        $._R,
        $._A,
        $._C,
        $._T
      ),
    END_UNSTRING: ($) =>
      seq(
        $._E,
        $._N,
        $._D,
        $._MINUSCHAR,
        $._U,
        $._N,
        $._S,
        $._T,
        $._R,
        $._I,
        $._N,
        $._G
      ),
    END_WRITE: ($) =>
      seq($._E, $._N, $._D, $._MINUSCHAR, $._W, $._R, $._I, $._T, $._E),
    //ENDING: ($) => seq($._E, $._N, $._D, $._I, $._N, $._F),
    ENTER: ($) => seq($._E, $._N, $._T, $._E, $._R),
    ENTRY: ($) => seq($._E, $._N, $._T, $._R, $._Y),
    ENTRY_PROCEDURE: ($) =>
      seq(
        $._E,
        $._N,
        $._T,
        $._R,
        $._Y,
        $._MINUSCHAR,
        $._P,
        $._R,
        $._O,
        $._C,
        $._E,
        $._D,
        $._U,
        $._R,
        $._E
      ),
    ENVIRONMENT: ($) =>
      seq($._E, $._N, $._V, $._I, $._R, $._O, $._N, $._M, $._E, $._N, $._T),
    EOP: ($) => seq($._E, $._O, $._P),
    EQUAL: ($) => seq($._E, $._Q, $._U, $._A, $._L),
    ERASE: ($) => seq($._E, $._R, $._A, $._S, $._E),
    ERROR: ($) => seq($._E, $._R, $._R, $._O, $._R),
    EOL: ($) => seq($._E, $._O, $._L),
    EOS: ($) => seq($._E, $._O, $._S),
    ESCAPE: ($) => seq($._E, $._S, $._C, $._A, $._P, $._E),
    ESI: ($) => seq($._E, $._S, $._I),
    EVALUATE: ($) => seq($._E, $._V, $._A, $._L, $._U, $._A, $._T, $._E),
    EVENT: ($) => seq($._E, $._V, $._E, $._N, $._T),
    EVERY: ($) => seq($._E, $._V, $._E, $._R, $._Y),
    EXCEPTION: ($) => seq($._E, $._X, $._C, $._E, $._P, $._T, $._I, $._O, $._N),
    EXCLUSIVE: ($) => seq($._E, $._X, $._C, $._L, $._U, $._S, $._I, $._V, $._E),
    EXHIBIT: ($) => seq($._E, $._X, $._H, $._I, $._B, $._I, $._T),
    EXIT: ($) => seq($._E, $._X, $._I, $._T),
    EXPORT: ($) => seq($._E, $._X, $._P, $._O, $._R, $._T),
    EXTEND: ($) => seq($._E, $._X, $._T, $._E, $._N, $._D),
    EXTENDED: ($) => seq($._E, $._X, $._T, $._E, $._N, $._D, $._E, $._D),
    EXTERNAL: ($) => seq($._E, $._X, $._T, $._E, $._R, $._N, $._A, $._L),
    FALSE: ($) => seq($._F, $._A, $._L, $._S, $._E),
    FD: ($) => seq($._F, $._D),
    FILE: ($) => seq($._F, $._I, $._L, $._E),
    FILE_CONTROL: ($) =>
      seq(
        $._F,
        $._I,
        $._L,
        $._E,
        $._MINUSCHAR,
        $._C,
        $._O,
        $._N,
        $._T,
        $._R,
        $._O,
        $._L
      ),
    FILLER: ($) => seq($._F, $._I, $._L, $._L, $._E, $._R),
    FINAL: ($) => seq($._F, $._I, $._N, $._A, $._L),
    FIRST: ($) => seq($._F, $._I, $._R, $._S, $._T),
    FOOTING: ($) => seq($._F, $._O, $._O, $._T, $._I, $._N, $._G),
    FOR: ($) => seq($._F, $._O, $._R),
    FOREGROUND_COLOR: ($) =>
      prec(
        2,
        seq(
          $._F,
          $._O,
          $._R,
          $._E,
          $._G,
          $._R,
          $._O,
          $._U,
          $._N,
          $._D,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._L,
          $._O,
          $._R
        )
      ),
    FOREGROUND_COLOUR: ($) =>
      prec(
        2,
        seq(
          $._F,
          $._O,
          $._R,
          $._E,
          $._G,
          $._R,
          $._O,
          $._U,
          $._N,
          $._D,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._L,
          $._O,
          $._U,
          $._R
        )
      ),
    FROM: ($) => seq($._F, $._R, $._O, $._M),
    FULL: ($) => seq($._F, $._U, $._L, $._L),
    FUNCTION: ($) => seq($._F, $._U, $._N, $._C, $._T, $._I, $._O, $._N),
    FUNCTIONNAME: ($) =>
      prec(
        2,
        seq(
          $._F,
          $._U,
          $._N,
          $._C,
          $._T,
          $._I,
          $._O,
          $._N,
          $._N,
          $._A,
          $._M,
          $._E
        )
      ),
    FUNCTION_POINTER: ($) =>
      prec(
        2,
        seq(
          $._F,
          $._U,
          $._N,
          $._C,
          $._T,
          $._I,
          $._O,
          $._N,
          $._MINUSCHAR,
          $._P,
          $._O,
          $._I,
          $._N,
          $._T,
          $._E,
          $._R
        )
      ),
    GENERATE: ($) => seq($._G, $._E, $._N, $._E, $._R, $._A, $._T, $._E),
    GOBACK: ($) => prec(2, seq($._G, $._O, $._B, $._A, $._C, $._K)),
    GIVING: ($) => seq($._G, $._I, $._V, $._I, $._N, $._G),
    GLOBAL: ($) => seq($._G, $._L, $._O, $._B, $._A, $._L),
    GO: ($) => seq($._G, $._O),
    GREATER: ($) => seq($._G, $._R, $._E, $._A, $._T, $._E, $._R),
    GRID: ($) => seq($._G, $._R, $._I, $._D),
    GROUP: ($) => seq($._G, $._R, $._O, $._U, $._P),
    HEADING: ($) => seq($._H, $._E, $._A, $._D, $._I, $._N, $._G),
    HIGHLIGHT: ($) => seq($._H, $._I, $._G, $._H, $._L, $._I, $._G, $._H, $._T),
    HIGH_VALUE: ($) =>
      seq($._H, $._I, $._G, $._H, $._MINUSCHAR, $._V, $._A, $._L, $._U, $._E),
    HIGH_VALUES: ($) =>
      prec(
        2,
        seq(
          $._H,
          $._I,
          $._G,
          $._H,
          $._MINUSCHAR,
          $._V,
          $._A,
          $._L,
          $._U,
          $._E,
          $._S
        )
      ),
    I_O: ($) => seq($._I, $._MINUSCHAR, $._O),
    I_O_CONTROL: ($) =>
      prec(
        2,
        seq(
          $._I,
          $._MINUSCHAR,
          $._O,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._N,
          $._T,
          $._R,
          $._O,
          $._L
        )
      ),
    ID: ($) => seq($._I, $._D),
    IDENTIFICATION: ($) =>
      prec(
        2,
        seq(
          $._I,
          $._D,
          $._E,
          $._N,
          $._T,
          $._I,
          $._F,
          $._I,
          $._C,
          $._A,
          $._T,
          $._I,
          $._O,
          $._N
        )
      ),
    IF: ($) => seq($._I, $._F),
    IMPLICIT: ($) => seq($._I, $._M, $._P, $._L, $._I, $._C, $._I, $._T),
    IMPORT: ($) => seq($._I, $._M, $._P, $._O, $._R, $._T),
    IN: ($) => seq($._I, $._N),
    INDEX: ($) => seq($._I, $._N, $._D, $._E, $._X),
    INDEXED: ($) => seq($._I, $._N, $._D, $._E, $._X, $._E, $._D),
    INDICATE: ($) => seq($._I, $._N, $._D, $._I, $._C, $._A, $._T, $._E),
    INITIAL: ($) => seq($._I, $._N, $._I, $._T, $._I, $._A, $._L),
    INITIALIZE: ($) =>
      seq($._I, $._N, $._I, $._T, $._I, $._A, $._L, $._I, $._Z, $._E),
    INITIATE: ($) => seq($._I, $._N, $._I, $._T, $._I, $._A, $._T, $._E),
    INPUT: ($) => seq($._I, $._N, $._P, $._U, $._T),
    INPUT_OUTPUT: ($) =>
      seq(
        $._I,
        $._N,
        $._P,
        $._U,
        $._T,
        $._MINUSCHAR,
        $._O,
        $._U,
        $._T,
        $._P,
        $._U,
        $._T
      ),
    INSPECT: ($) => seq($._I, $._N, $._S, $._P, $._E, $._C, $._T),
    INSTALLATION: ($) =>
      seq(
        $._I,
        $._N,
        $._S,
        $._T,
        $._A,
        $._L,
        $._L,
        $._A,
        $._T,
        $._I,
        $._O,
        $._N
      ),
    INTEGER: ($) => prec(2, seq($._I, $._N, $._T, $._E, $._G, $._E, $._R)),
    INTO: ($) => seq($._I, $._N, $._T, $._O),
    INVALID: ($) => seq($._I, $._N, $._V, $._A, $._L, $._I, $._D),
    INVOKE: ($) => seq($._I, $._N, $._V, $._O, $._K, $._E),
    IS: ($) => seq($._I, $._S),
    JUST: ($) => seq($._J, $._U, $._S, $._T),
    JUSTIFIED: ($) =>
      prec(2, seq($._J, $._U, $._S, $._T, $._I, $._F, $._I, $._E, $._D)),
    KANJI: ($) => seq($._K, $._A, $._N, $._J, $._I),
    KEPT: ($) => seq($._K, $._E, $._P, $._T),
    KEY: ($) => seq($._K, $._E, $._Y),
    KEYBOARD: ($) =>
      prec(2, seq($._K, $._E, $._Y, $._B, $._O, $._A, $._R, $._D)),
    LABEL: ($) => seq($._L, $._A, $._B, $._E, $._L),
    LANGUAGE: ($) => seq($._L, $._A, $._N, $._G, $._U, $._A, $._G, $._E),
    LAST: ($) => seq($._L, $._A, $._S, $._T),
    LB: ($) => seq($._L, $._B),
    LD: ($) => seq($._L, $._D),
    LEADING: ($) => seq($._L, $._E, $._A, $._D, $._I, $._N, $._G),
    LEFT: ($) => seq($._L, $._E, $._F, $._T),
    LEFTLINE: ($) =>
      prec(2, seq($._L, $._E, $._F, $._T, $._L, $._I, $._N, $._E)),
    LENGTH: ($) => seq($._L, $._E, $._N, $._G, $._T, $._H),
    LENGTH_CHECK: ($) =>
      prec(
        2,
        seq(
          $._L,
          $._E,
          $._N,
          $._G,
          $._T,
          $._H,
          $._MINUSCHAR,
          $._C,
          $._H,
          $._E,
          $._C,
          $._K
        )
      ),
    LESS: ($) => seq($._L, $._E, $._S, $._S),
    LIBACCESS: ($) => seq($._L, $._I, $._B, $._A, $._C, $._C, $._E, $._S, $._S),
    LIBPARAMETER: ($) =>
      seq(
        $._L,
        $._I,
        $._B,
        $._P,
        $._A,
        $._R,
        $._A,
        $._M,
        $._E,
        $._T,
        $._E,
        $._R
      ),
    LIBRARY: ($) => seq($._L, $._I, $._B, $._R, $._A, $._R, $._Y),
    LIMIT: ($) => seq($._L, $._I, $._M, $._I, $._T),
    LIMITS: ($) => prec(2, seq($._L, $._I, $._M, $._I, $._T, $._S)),
    LINAGE: ($) => seq($._L, $._I, $._N, $._A, $._G, $._E),
    LINAGE_COUNTER: ($) =>
      prec(
        2,
        seq(
          $._L,
          $._I,
          $._N,
          $._A,
          $._G,
          $._E,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._U,
          $._N,
          $._T,
          $._E,
          $._R
        )
      ),
    LINE: ($) => seq($._L, $._I, $._N, $._E),
    LINES: ($) => prec(2, seq($._L, $._I, $._N, $._E, $._S)),
    LINE_COUNTER: ($) =>
      prec(
        2,
        seq(
          $._L,
          $._I,
          $._N,
          $._E,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._U,
          $._N,
          $._T,
          $._E,
          $._R
        )
      ),
    LINKAGE: ($) => seq($._L, $._I, $._N, $._K, $._A, $._G, $._E),
    LIST: ($) => seq($._L, $._I, $._S, $._T),
    LOCAL: ($) => seq($._L, $._O, $._C, $._A, $._L),
    LOCAL_STORAGE: ($) =>
      prec(
        2,
        seq(
          $._L,
          $._O,
          $._C,
          $._A,
          $._L,
          $._MINUSCHAR,
          $._S,
          $._T,
          $._O,
          $._R,
          $._A,
          $._G,
          $._E
        )
      ),
    LOCK: ($) => seq($._L, $._O, $._C, $._K),
    LONG_DATE: ($) =>
      seq($._L, $._O, $._N, $._G, $._MINUSCHAR, $._D, $._A, $._T, $._E),
    LONG_TIME: ($) =>
      seq($._L, $._O, $._N, $._G, $._MINUSCHAR, $._T, $._I, $._M, $._E),
    LOWER: ($) => seq($._L, $._O, $._W, $._E, $._R),
    LOWLIGHT: ($) => seq($._L, $._O, $._W, $._L, $._I, $._G, $._H, $._T),
    LOW_VALUE: ($) =>
      seq($._L, $._O, $._W, $._MINUSCHAR, $._V, $._A, $._L, $._U, $._E),
    LOW_VALUES: ($) =>
      prec(
        2,
        seq($._L, $._O, $._W, $._MINUSCHAR, $._V, $._A, $._L, $._U, $._E, $._S)
      ),
    MEMORY: ($) => seq($._M, $._E, $._M, $._O, $._R, $._Y),
    MERGE: ($) => seq($._M, $._E, $._R, $._G, $._E),
    MESSAGE: ($) => seq($._M, $._E, $._S, $._S, $._A, $._G, $._E),
    MMDDYYYY: ($) => seq($._M, $._M, $._D, $._D, $._Y, $._Y, $._Y, $._Y),
    MODE: ($) => seq($._M, $._O, $._D, $._E),
    MODULES: ($) => seq($._M, $._O, $._D, $._U, $._L, $._E, $._S),
    MORE_LABELS: ($) =>
      seq(
        $._M,
        $._O,
        $._R,
        $._E,
        $._MINUSCHAR,
        $._L,
        $._A,
        $._B,
        $._E,
        $._L,
        $._S
      ),
    MOVE: ($) => seq($._M, $._O, $._V, $._E),
    MULTIPLE: ($) => seq($._M, $._U, $._L, $._T, $._I, $._P, $._L, $._E),
    MULTIPLY: ($) => seq($._M, $._U, $._L, $._T, $._I, $._P, $._L, $._Y),
    NAMED: ($) => seq($._N, $._A, $._M, $._E, $._D),
    NATIONAL: ($) => seq($._N, $._A, $._T, $._I, $._O, $._N, $._A, $._L),
    NATIONAL_EDITED: ($) =>
      prec(
        2,
        seq(
          $._N,
          $._A,
          $._T,
          $._I,
          $._O,
          $._N,
          $._A,
          $._L,
          $._MINUSCHAR,
          $._E,
          $._D,
          $._I,
          $._T,
          $._E,
          $._D
        )
      ),
    NATIVE: ($) => seq($._N, $._A, $._T, $._I, $._V, $._E),
    NEGATIVE: ($) => seq($._N, $._E, $._G, $._A, $._T, $._I, $._V, $._E),
    NETWORK: ($) => seq($._N, $._E, $._T, $._W, $._O, $._R, $._K),
    NEXT: ($) => seq($._N, $._E, $._X, $._T),
    NO: ($) => seq($._N, $._O),
    NO_ECHO: ($) =>
      prec(2, seq($._N, $._O, $._MINUSCHAR, $._E, $._C, $._H, $._O)),
    NOT: ($) => seq($._N, $._O, $._T),
    NULL: ($) => seq($._N, $._U, $._L, $._L),
    NULLS: ($) => prec(2, seq($._N, $._U, $._L, $._L, $._S)),
    NUMBER: ($) => seq($._N, $._U, $._M, $._B, $._E, $._R),
    NUMERIC: ($) => seq($._N, $._U, $._M, $._E, $._R, $._I, $._C),
    NUMERIC_DATE: ($) =>
      prec(
        2,
        seq(
          $._N,
          $._U,
          $._M,
          $._E,
          $._R,
          $._I,
          $._C,
          $._MINUSCHAR,
          $._D,
          $._A,
          $._T,
          $._E
        )
      ),
    NUMERIC_EDITED: ($) =>
      prec(
        2,
        seq(
          $._N,
          $._U,
          $._M,
          $._E,
          $._R,
          $._I,
          $._C,
          $._MINUSCHAR,
          $._E,
          $._D,
          $._I,
          $._T,
          $._E,
          $._D
        )
      ),
    NUMERIC_TIME: ($) =>
      prec(
        2,
        seq(
          $._N,
          $._U,
          $._M,
          $._E,
          $._R,
          $._I,
          $._C,
          $._MINUSCHAR,
          $._T,
          $._I,
          $._M,
          $._E
        )
      ),
    OBJECT_COMPUTER: ($) =>
      seq(
        $._O,
        $._B,
        $._J,
        $._E,
        $._C,
        $._T,
        $._MINUSCHAR,
        $._C,
        $._O,
        $._M,
        $._P,
        $._U,
        $._T,
        $._E,
        $._R
      ),
    OCCURS: ($) => seq($._O, $._C, $._C, $._U, $._R, $._S),
    ODT: ($) => seq($._O, $._D, $._T),
    OF: ($) => seq($._O, $._F),
    OFF: ($) => prec(2, seq($._O, $._F, $._F)),
    OMITTED: ($) => seq($._O, $._M, $._I, $._T, $._T, $._E, $._D),
    ON: ($) => seq($._O, $._N),
    OPEN: ($) => seq($._O, $._P, $._E, $._N),
    OPTIONAL: ($) => seq($._O, $._P, $._T, $._I, $._O, $._N, $._A, $._L),
    OR: ($) => seq($._O, $._R),
    ORDER: ($) => prec(2, seq($._O, $._R, $._D, $._E, $._R)),
    ORDERLY: ($) => prec(3, seq($._O, $._R, $._D, $._E, $._R, $._L, $._Y)),
    ORGANIZATION: ($) =>
      prec(
        2,
        seq(
          $._O,
          $._R,
          $._G,
          $._A,
          $._N,
          $._I,
          $._Z,
          $._A,
          $._T,
          $._I,
          $._O,
          $._N
        )
      ),
    OTHER: ($) => seq($._O, $._T, $._H, $._E, $._R),
    OUTPUT: ($) => seq($._O, $._U, $._T, $._P, $._U, $._T),
    OVERFLOW: ($) => seq($._O, $._V, $._E, $._R, $._F, $._L, $._O, $._W),
    OVERLINE: ($) => seq($._O, $._V, $._E, $._R, $._L, $._I, $._N, $._E),
    OWN: ($) => seq($._O, $._W, $._N),
    PACKED_DECIMAL: ($) =>
      seq(
        $._P,
        $._A,
        $._C,
        $._K,
        $._E,
        $._D,
        $._MINUSCHAR,
        $._D,
        $._E,
        $._C,
        $._I,
        $._M,
        $._A,
        $._L
      ),
    PADDING: ($) => seq($._P, $._A, $._D, $._D, $._I, $._N, $._G),
    PAGE: ($) => seq($._P, $._A, $._G, $._E),
    PAGE_COUNTER: ($) =>
      seq(
        $._P,
        $._A,
        $._G,
        $._E,
        $._MINUSCHAR,
        $._C,
        $._O,
        $._U,
        $._N,
        $._T,
        $._E,
        $._R
      ),
    PASSWORD: ($) => seq($._P, $._A, $._S, $._S, $._W, $._O, $._R, $._D),
    PERFORM: ($) => seq($._P, $._E, $._R, $._F, $._O, $._R, $._M),
    PF: ($) => seq($._P, $._F),
    PH: ($) => seq($._P, $._H),
    PIC: ($) => seq($._P, $._I, $._C),
    PICTURE: ($) => prec(2, seq($._P, $._I, $._C, $._T, $._U, $._R, $._E)),
    PLUS: ($) => seq($._P, $._L, $._U, $._S),
    POINTER: ($) => seq($._P, $._O, $._I, $._N, $._T, $._E, $._R),
    POSITION: ($) => seq($._P, $._O, $._S, $._I, $._T, $._I, $._O, $._N),
    POSITIVE: ($) => seq($._P, $._O, $._S, $._I, $._T, $._I, $._V, $._E),
    PORT: ($) => seq($._P, $._O, $._R, $._T),
    PRINTER: ($) => seq($._P, $._R, $._I, $._N, $._T, $._E, $._R),
    //PRINTING: ($) => seq($._P, $._R, $._I, $._N, $._T, $._I, $._N, $._G),
    PRIVATE: ($) => seq($._P, $._R, $._I, $._V, $._A, $._T, $._E),
    PROCEDURE: ($) => seq($._P, $._R, $._O, $._C, $._E, $._D, $._U, $._R, $._E),
    PROCEDURE_POINTER: ($) =>
      prec(
        2,
        seq(
          $._P,
          $._R,
          $._O,
          $._C,
          $._E,
          $._D,
          $._U,
          $._R,
          $._E,
          $._MINUSCHAR,
          $._P,
          $._O,
          $._I,
          $._N,
          $._T,
          $._E,
          $._R
        )
      ),
    PROCEDURES: ($) =>
      prec(2, seq($._P, $._R, $._O, $._C, $._E, $._D, $._U, $._R, $._E, $._S)),
    PROCEED: ($) => seq($._P, $._R, $._O, $._C, $._E, $._E, $._D),
    PROCESS: ($) => seq($._P, $._R, $._O, $._C, $._E, $._S, $._S),
    PROGRAM: ($) => seq($._P, $._R, $._O, $._G, $._R, $._A, $._M),
    PROGRAM_ID: ($) =>
      prec(
        2,
        seq($._P, $._R, $._O, $._G, $._R, $._A, $._M, $._MINUSCHAR, $._I, $._D)
      ),
    PROGRAM_LIBRARY: ($) =>
      prec(
        2,
        seq(
          $._P,
          $._R,
          $._O,
          $._G,
          $._R,
          $._A,
          $._M,
          $._MINUSCHAR,
          $._L,
          $._I,
          $._B,
          $._R,
          $._A,
          $._R,
          $._Y
        )
      ),
    PROMPT: ($) => seq($._P, $._R, $._O, $._M, $._P, $._T),
    PURGE: ($) => seq($._P, $._U, $._R, $._G, $._E),
    QUEUE: ($) => seq($._Q, $._U, $._E, $._U, $._E),
    QUOTE: ($) => seq($._Q, $._U, $._O, $._T, $._E),
    QUOTES: ($) => prec(2, seq($._Q, $._U, $._O, $._T, $._E, $._S)),
    RANDOM: ($) => seq($._R, $._A, $._N, $._D, $._O, $._M),
    READER: ($) => prec(2, seq($._R, $._E, $._A, $._D, $._E, $._R)),
    REMOTE: ($) => seq($._R, $._E, $._M, $._O, $._T, $._E),
    RD: ($) => seq($._R, $._D),
    REAL: ($) => seq($._R, $._E, $._A, $._L),
    READ: ($) => seq($._R, $._E, $._A, $._D),
    RECEIVE: ($) => seq($._R, $._E, $._C, $._E, $._I, $._V, $._E),
    RECEIVED: ($) =>
      prec(2, seq($._R, $._E, $._C, $._E, $._I, $._V, $._E, $._D)),
    RECORD: ($) => seq($._R, $._E, $._C, $._O, $._R, $._D),
    RECORDING: ($) =>
      prec(2, seq($._R, $._E, $._C, $._O, $._R, $._D, $._I, $._N, $._G)),
    RECORDS: ($) => prec(2, seq($._R, $._E, $._C, $._O, $._R, $._D, $._S)),
    RECURSIVE: ($) => seq($._R, $._E, $._C, $._U, $._R, $._S, $._I, $._V, $._E),
    REDEFINES: ($) => seq($._R, $._E, $._D, $._E, $._F, $._I, $._N, $._E, $._S),
    REEL: ($) => seq($._R, $._E, $._E, $._L),
    REF: ($) => seq($._R, $._E, $._F),
    REFERENCE: ($) =>
      prec(2, seq($._R, $._E, $._F, $._E, $._R, $._E, $._N, $._C, $._E)),
    REFERENCES: ($) =>
      prec(3, seq($._R, $._E, $._F, $._E, $._R, $._E, $._N, $._C, $._E, $._S)),
    RELATIVE: ($) => seq($._R, $._E, $._L, $._A, $._T, $._I, $._V, $._E),
    RELEASE: ($) => seq($._R, $._E, $._L, $._E, $._A, $._S, $._E),
    REMAINDER: ($) => seq($._R, $._E, $._M, $._A, $._I, $._N, $._D, $._E, $._R),
    REMARKS: ($) => seq($._R, $._E, $._M, $._A, $._R, $._K, $._S),
    REMOVAL: ($) => seq($._R, $._E, $._M, $._O, $._V, $._A, $._L),
    REMOVE: ($) => seq($._R, $._E, $._M, $._O, $._V, $._E),
    RENAMES: ($) => seq($._R, $._E, $._N, $._A, $._M, $._E, $._S),
    REPLACE: ($) => seq($._R, $._E, $._P, $._L, $._A, $._C, $._E),
    REPLACING: ($) => seq($._R, $._E, $._P, $._L, $._A, $._C, $._I, $._N, $._G),
    REPORT: ($) => seq($._R, $._E, $._P, $._O, $._R, $._T),
    //REPORTING: ($) => seq($._R, $._E, $._P, $._O, $._R, $._T, $._I, $._N, $._G),
    REPORTS: ($) => prec(2, seq($._R, $._E, $._P, $._O, $._R, $._T, $._S)),
    REQUIRED: ($) => seq($._R, $._E, $._Q, $._U, $._I, $._R, $._E, $._D),
    RERUN: ($) => seq($._R, $._E, $._R, $._U, $._N),
    RESERVE: ($) => seq($._R, $._E, $._S, $._E, $._R, $._V, $._E),
    REVERSE_VIDEO: ($) =>
      seq(
        $._R,
        $._E,
        $._S,
        $._E,
        $._R,
        $._V,
        $._E,
        $._MINUSCHAR,
        $._V,
        $._I,
        $._D,
        $._E,
        $._O
      ),
    RESET: ($) => seq($._R, $._E, $._S, $._E, $._T),
    RETURN: ($) => seq($._R, $._E, $._T, $._U, $._R, $._N),
    RETURN_CODE: ($) =>
      prec(
        2,
        seq(
          $._R,
          $._E,
          $._T,
          $._U,
          $._R,
          $._N,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._D,
          $._E
        )
      ),
    RETURNING: ($) => seq($._R, $._E, $._T, $._U, $._R, $._N, $._I, $._N, $._G),
    REVERSED: ($) => seq($._R, $._E, $._V, $._E, $._R, $._S, $._E, $._D),
    REWIND: ($) => seq($._R, $._E, $._W, $._I, $._N, $._D),
    REWRITE: ($) => seq($._R, $._E, $._W, $._R, $._I, $._T, $._E),
    RF: ($) => seq($._R, $._F),
    RH: ($) => seq($._R, $._H),
    RIGHT: ($) => seq($._R, $._I, $._G, $._H, $._T),
    ROUNDED: ($) => seq($._R, $._O, $._U, $._N, $._D, $._E, $._D),
    RUN: ($) => seq($._R, $._U, $._N),
    SAME: ($) => seq($._S, $._A, $._M, $._E),
    SAVE: ($) => seq($._S, $._A, $._V, $._E),
    SCREEN: ($) => seq($._S, $._C, $._R, $._E, $._E, $._N),
    SD: ($) => seq($._S, $._D),
    SEARCH: ($) => seq($._S, $._E, $._A, $._R, $._C, $._H),
    SECTION: ($) => seq($._S, $._E, $._C, $._T, $._I, $._O, $._N),
    SECURE: ($) => seq($._S, $._E, $._C, $._U, $._R, $._E),
    SECURITY: ($) => seq($._S, $._E, $._C, $._U, $._R, $._I, $._T, $._Y),
    SEGMENT: ($) => seq($._S, $._E, $._G, $._M, $._E, $._N, $._T),
    SEGMENT_LIMIT: ($) =>
      seq(
        $._S,
        $._E,
        $._G,
        $._M,
        $._E,
        $._N,
        $._T,
        $._MINUSCHAR,
        $._L,
        $._I,
        $._M,
        $._I,
        $._T
      ),
    SELECT: ($) => seq($._S, $._E, $._L, $._E, $._C, $._T),
    SEND: ($) => seq($._S, $._E, $._N, $._D),
    SENTENCE: ($) => seq($._S, $._E, $._N, $._T, $._E, $._N, $._C, $._E),
    SEPARATE: ($) => seq($._S, $._E, $._P, $._A, $._R, $._A, $._T, $._E),
    SEQUENCE: ($) => seq($._S, $._E, $._Q, $._U, $._E, $._N, $._C, $._E),
    SEQUENTIAL: ($) =>
      seq($._S, $._E, $._Q, $._U, $._E, $._N, $._T, $._I, $._A, $._L),
    SET: ($) => seq($._S, $._E, $._T),
    SHARED: ($) => seq($._S, $._H, $._A, $._R, $._E, $._D),
    SHAREDBYALL: ($) =>
      prec(
        2,
        seq($._S, $._H, $._A, $._R, $._E, $._D, $._B, $._Y, $._A, $._L, $._L)
      ),
    SHAREDBYRUNUNIT: ($) =>
      prec(
        2,
        seq(
          $._S,
          $._H,
          $._A,
          $._R,
          $._E,
          $._D,
          $._B,
          $._Y,
          $._R,
          $._U,
          $._N,
          $._U,
          $._N,
          $._I,
          $._T
        )
      ),
    SHARING: ($) => seq($._S, $._H, $._A, $._R, $._I, $._N, $._G),
    SHIFT_IN: ($) =>
      seq($._S, $._H, $._I, $._F, $._T, $._MINUSCHAR, $._I, $._N),
    SHIFT_OUT: ($) =>
      seq($._S, $._H, $._I, $._F, $._T, $._MINUSCHAR, $._O, $._U, $._T),
    SHORT_DATE: ($) =>
      seq($._S, $._H, $._O, $._R, $._T, $._MINUSCHAR, $._D, $._A, $._T, $._E),
    SIGN: ($) => seq($._S, $._I, $._G, $._N),
    SIZE: ($) => seq($._S, $._I, $._Z, $._E),
    SORT: ($) => seq($._S, $._O, $._R, $._T),
    SORT_CONTROL: ($) =>
      prec(
        2,
        seq(
          $._S,
          $._O,
          $._R,
          $._T,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._N,
          $._T,
          $._R,
          $._O,
          $._L
        )
      ),
    SORT_CORE_SIZE: ($) =>
      prec(
        2,
        seq(
          $._S,
          $._O,
          $._R,
          $._T,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._R,
          $._E,
          $._MINUSCHAR,
          $._S,
          $._I,
          $._Z,
          $._E
        )
      ),
    SORT_FILE_SIZE: ($) =>
      prec(
        2,
        seq(
          $._S,
          $._O,
          $._R,
          $._T,
          $._MINUSCHAR,
          $._F,
          $._I,
          $._L,
          $._E,
          $._MINUSCHAR,
          $._S,
          $._I,
          $._Z,
          $._E
        )
      ),
    SORT_MERGE: ($) =>
      prec(
        2,
        seq($._S, $._O, $._R, $._T, $._MINUSCHAR, $._M, $._E, $._R, $._G, $._E)
      ),
    SORT_MESSAGE: ($) =>
      prec(
        2,
        seq(
          $._S,
          $._O,
          $._R,
          $._T,
          $._MINUSCHAR,
          $._M,
          $._E,
          $._S,
          $._S,
          $._A,
          $._G,
          $._E
        )
      ),
    SORT_MODE_SIZE: ($) =>
      prec(
        2,
        seq(
          $._S,
          $._O,
          $._R,
          $._T,
          $._MINUSCHAR,
          $._M,
          $._O,
          $._D,
          $._E,
          $._MINUSCHAR,
          $._S,
          $._I,
          $._Z,
          $._E
        )
      ),
    SORT_RETURN: ($) =>
      prec(
        2,
        seq(
          $._S,
          $._O,
          $._R,
          $._T,
          $._MINUSCHAR,
          $._R,
          $._E,
          $._T,
          $._U,
          $._R,
          $._N
        )
      ),
    SOURCE: ($) => seq($._S, $._O, $._U, $._R, $._C, $._E),
    SOURCE_COMPUTER: ($) =>
      prec(
        2,
        seq(
          $._S,
          $._O,
          $._U,
          $._R,
          $._C,
          $._E,
          $._MINUSCHAR,
          $._C,
          $._O,
          $._M,
          $._P,
          $._U,
          $._T,
          $._E,
          $._R
        )
      ),
    SPACE: ($) => seq($._S, $._P, $._A, $._C, $._E),
    SPACES: ($) => prec(2, seq($._S, $._P, $._A, $._C, $._E, $._S)),
    SPECIAL_NAMES: ($) =>
      seq(
        $._S,
        $._P,
        $._E,
        $._C,
        $._I,
        $._A,
        $._L,
        $._MINUSCHAR,
        $._N,
        $._A,
        $._M,
        $._E,
        $._S
      ),
    SQL: ($) => seq($._S, $._Q, $._L),
    STANDARD: ($) => seq($._S, $._T, $._A, $._N, $._D, $._A, $._R, $._D),
    STANDARD_1: ($) =>
      seq($._S, $._T, $._A, $._N, $._D, $._A, $._R, $._D, $._MINUSCHAR, "1"),
    STANDARD_2: ($) =>
      seq($._S, $._T, $._A, $._N, $._D, $._A, $._R, $._D, $._MINUSCHAR, "2"),
    START: ($) => seq($._S, $._T, $._A, $._R, $._T),
    STATUS: ($) => seq($._S, $._T, $._A, $._T, $._U, $._S),
    STOP: ($) => seq($._S, $._T, $._O, $._P),
    STRING: ($) => seq($._S, $._T, $._R, $._I, $._N, $._G),
    SUB_QUEUE_1: ($) =>
      seq(
        $._S,
        $._U,
        $._B,
        $._MINUSCHAR,
        $._Q,
        $._U,
        $._E,
        $._U,
        $._E,
        $._MINUSCHAR,
        "1"
      ),
    SUB_QUEUE_2: ($) =>
      seq(
        $._S,
        $._U,
        $._B,
        $._MINUSCHAR,
        $._Q,
        $._U,
        $._E,
        $._U,
        $._E,
        $._MINUSCHAR,
        "2"
      ),
    SUB_QUEUE_3: ($) =>
      seq(
        $._S,
        $._U,
        $._B,
        $._MINUSCHAR,
        $._Q,
        $._U,
        $._E,
        $._U,
        $._E,
        $._MINUSCHAR,
        "3"
      ),
    SUBTRACT: ($) => seq($._S, $._U, $._B, $._T, $._R, $._A, $._C, $._T),
    SUM: ($) => seq($._S, $._U, $._M),
    //SUPPRESS: ($) => seq($._S, $._U, $._P, $._P, $._R, $._E, $._S, $._S),
    SYMBOL: ($) => seq($._S, $._Y, $._M, $._B, $._O, $._L),
    SYMBOLIC: ($) =>
      prec(2, seq($._S, $._Y, $._M, $._B, $._O, $._L, $._I, $._C)),
    SYNC: ($) => seq($._S, $._Y, $._N, $._C),
    SYNCHRONIZED: ($) =>
      prec(
        2,
        seq(
          $._S,
          $._Y,
          $._N,
          $._C,
          $._H,
          $._R,
          $._O,
          $._N,
          $._I,
          $._Z,
          $._E,
          $._D
        )
      ),
    TABLE: ($) => seq($._T, $._A, $._B, $._L, $._E),
    TALLY: ($) => seq($._T, $._A, $._L, $._L, $._Y),
    TALLYING: ($) =>
      prec(2, seq($._T, $._A, $._L, $._L, $._Y, $._I, $._N, $._G)),
    TASK: ($) => seq($._T, $._A, $._S, $._K),
    TAPE: ($) => seq($._T, $._A, $._P, $._E),
    TERMINAL: ($) => seq($._T, $._E, $._R, $._M, $._I, $._N, $._A, $._L),
    TERMINATE: ($) => seq($._T, $._E, $._R, $._M, $._I, $._N, $._A, $._T, $._E),
    TEST: ($) => seq($._T, $._E, $._S, $._T),
    TEXT: ($) => seq($._T, $._E, $._X, $._T),
    THAN: ($) => seq($._T, $._H, $._A, $._N),
    THEN: ($) => seq($._T, $._H, $._E, $._N),
    THREAD: ($) => seq($._T, $._H, $._R, $._E, $._A, $._D),
    THREAD_LOCAL: ($) =>
      prec(
        2,
        seq(
          $._T,
          $._H,
          $._R,
          $._E,
          $._A,
          $._D,
          $._MINUSCHAR,
          $._L,
          $._O,
          $._C,
          $._A,
          $._L
        )
      ),
    THROUGH: ($) => seq($._T, $._H, $._R, $._O, $._U, $._G, $._H),
    THRU: ($) => seq($._T, $._H, $._R, $._U),
    TIME: ($) => seq($._T, $._I, $._M, $._E),
    TIMER: ($) => prec(2, seq($._T, $._I, $._M, $._E, $._R)),
    TIMES: ($) => prec(2, seq($._T, $._I, $._M, $._E, $._S)),
    TITLE: ($) => seq($._T, $._I, $._T, $._L, $._E),
    TO: ($) => seq($._T, $._O),
    TODAYS_DATE: ($) =>
      prec(
        2,
        seq(
          $._T,
          $._O,
          $._D,
          $._A,
          $._Y,
          $._S,
          $._MINUSCHAR,
          $._D,
          $._A,
          $._T,
          $._E
        )
      ),
    TODAYS_NAME: ($) =>
      prec(
        2,
        seq(
          $._T,
          $._O,
          $._D,
          $._A,
          $._Y,
          $._S,
          $._MINUSCHAR,
          $._N,
          $._A,
          $._M,
          $._E
        )
      ),
    TOP: ($) => seq($._T, $._O, $._P),
    TRAILING: ($) => seq($._T, $._R, $._A, $._I, $._L, $._I, $._N, $._G),
    TRUE: ($) => seq($._T, $._R, $._U, $._E),
    TRUNCATED: ($) => seq($._T, $._R, $._U, $._N, $._C, $._A, $._T, $._E, $._D),
    TYPE: ($) => seq($._T, $._Y, $._P, $._E),
    TYPEDEF: ($) => prec(2, seq($._T, $._Y, $._P, $._E, $._D, $._E, $._F)),
    UNDERLINE: ($) => seq($._U, $._N, $._D, $._E, $._R, $._L, $._I, $._N, $._E),
    UNIT: ($) => seq($._U, $._N, $._I, $._T),
    UNSTRING: ($) => seq($._U, $._N, $._S, $._T, $._R, $._I, $._N, $._G),
    UNTIL: ($) => seq($._U, $._N, $._T, $._I, $._L),
    UP: ($) => seq($._U, $._P),
    UPON: ($) => prec(2, seq($._U, $._P, $._O, $._N)),
    USAGE: ($) => seq($._U, $._S, $._A, $._G, $._E),
    USE: ($) => seq($._U, $._S, $._E),
    USING: ($) => seq($._U, $._S, $._I, $._N, $._G),
    VALUE: ($) => seq($._V, $._A, $._L, $._U, $._E),
    VALUES: ($) => prec(2, seq($._V, $._A, $._L, $._U, $._E, $._S)),
    VARYING: ($) => seq($._V, $._A, $._R, $._Y, $._I, $._N, $._G),
    VIRTUAL: ($) => seq($._V, $._I, $._R, $._T, $._U, $._A, $._L),
    WAIT: ($) => seq($._W, $._A, $._I, $._T),
    WHEN: ($) => seq($._W, $._H, $._E, $._N),
    WHEN_COMPILED: ($) =>
      seq(
        $._W,
        $._H,
        $._E,
        $._N,
        $._MINUSCHAR,
        $._C,
        $._O,
        $._M,
        $._P,
        $._I,
        $._L,
        $._E,
        $._D
      ),
    WITH: ($) => seq($._W, $._I, $._T, $._H),
    WORDS: ($) => seq($._W, $._O, $._R, $._D, $._S),
    WORKING_STORAGE: ($) =>
      seq(
        $._W,
        $._O,
        $._R,
        $._K,
        $._I,
        $._N,
        $._G,
        $._MINUSCHAR,
        $._S,
        $._T,
        $._O,
        $._R,
        $._A,
        $._G,
        $._E
      ),
    WRITE: ($) => seq($._W, $._R, $._I, $._T, $._E),
    YEAR: ($) => seq($._Y, $._E, $._A, $._R),
    YYYYMMDD: ($) => seq($._Y, $._Y, $._Y, $._Y, $._M, $._M, $._D, $._D),
    YYYYDDD: ($) => seq($._Y, $._Y, $._Y, $._Y, $._D, $._D, $._D),
    ZERO: ($) => seq($._Z, $._E, $._R, $._O),
    ZERO_FILL: ($) =>
      prec(
        2,
        seq($._Z, $._E, $._R, $._O, $._MINUSCHAR, $._F, $._I, $._L, $._L)
      ),
    ZEROS: ($) => prec(2, seq($._Z, $._E, $._R, $._O, $._S)),
    ZEROES: ($) => prec(2, seq($._Z, $._E, $._R, $._O, $._E, $._S)),
    // symbols
    //AMPCHAR: ($) => "&",
    ASTERISKCHAR: ($) => "*",
    DOUBLEASTERISKCHAR: ($) => "**",
    COLONCHAR: ($) => ":",
    COMMACHAR: ($) => ",",
    COMMENTENTRYTAG: ($) => "*>CE",
    COMMENTTAG: ($) => "*>",
    DOLLARCHAR: ($) => "$",
    // DOUBLEQUOTE: ($) => '"',
    // period full stop
    DOT_FS: ($) => seq(".", repeat1(choice("\r", "\n", "\f", "\t", " "))),
    DOT: ($) => ".",
    EQUALCHAR: ($) => "=",
    EXECCICSTAG: ($) => "*>EXECCICS",
    EXECSQLTAG: ($) => "*>EXECSQL",
    EXECSQLIMSTAG: ($) => "*>EXECSQLIMS",
    LESSTHANCHAR: ($) => "<",
    LESSTHANOREQUAL: ($) => "<=",
    LPARENCHAR: ($) => "(",
    _MINUSCHAR: ($) => "-",
    MORETHANCHAR: ($) => ">",
    MORETHANOREQUAL: ($) => ">=",
    NOTEQUALCHAR: ($) => "<>",
    PLUSCHAR: ($) => "+",
    SINGLEQUOTE: ($) => "'",
    RPARENCHAR: ($) => ")",
    SLASHCHAR: ($) => "/",
    // literals
    NONNUMERICLITERAL: ($) =>
      choice($._STRINGLITERAL, $._DBCSLITERAL, $._HEXNUMBER, $._NULLTERMINATED),
    _HEXNUMBER: ($) =>
      choice(
        seq($._X, '"', /[0-9A-F]+/, '"'),
        seq($._X, "'", /[0-9A-F]+/, "'")
      ),
    _NULLTERMINATED: ($) =>
      choice(
        seq($._Z, '"', repeat(choice(/[^"\n\r]/, '""', "'")), '"'),
        seq($._Z, "'", repeat(choice(/[^'\n\r]/, "''", '"')), "'")
      ),
    _STRINGLITERAL: ($) =>
      choice(
        seq('"', repeat(choice(/[^"\n\r]/, '""', "'")), '"'),
        seq("'", repeat(choice(/[^'\n\r]/, "''", '"')), "'")
      ),
    _DBCSLITERAL: ($) =>
      choice(
        seq(/[GN]/, '"', repeat(choice(/[^"\n\r]/, '""', "'")), '"'),
        seq(/[GN]/, "'", repeat(choice(/[^'\n\r]/, "''", '"')), "'")
      ),
    LEVEL_NUMBER_66: ($) => "66",
    LEVEL_NUMBER_77: ($) => "77",
    LEVEL_NUMBER_88: ($) => "88",
    INTEGERLITERAL: ($) =>
      seq(optional(choice($.PLUSCHAR, $._MINUSCHAR)), /[0-9]+/),
    NUMERICLITERAL: ($) =>
      seq(
        optional(choice($.PLUSCHAR, $._MINUSCHAR)),
        /[0-9]*/,
        choice($.DOT, $.COMMACHAR),
        /[0-9]+/,
        optional(
          seq(/[eE]/, optional(choice($.PLUSCHAR, $._MINUSCHAR)), /[0-9]+/)
        )
      ),
    IDENTIFIER: ($) => /[a-zA-Z0-9]+([-_]+[a-zA-Z0-9]+)*/,
    // whitespace, line breaks, comments, ...
    // NEWLINE: ($) => seq(optional("\r"), "\n"),
    EXECCICSLINE: ($) => seq($.EXECCICSTAG, $._WS, /[^\n\r}]*[\n\r}]/),
    EXECSQLIMSLINE: ($) => seq($.EXECSQLIMSTAG, $._WS, /[^\n\r}]*[\n\r}]/),
    EXECSQLLINE: ($) => seq($.EXECSQLTAG, $._WS, /[^\n\r}]*[\n\r}]/),
    // COMMENTLINE: ($) => seq($.COMMENTTAG, $.WS, /[^\n\r]*/),
    _WS: ($) => /[\t\f;]+/,
    // SEPARATOR: ($) => ", ",
    // case insensitive chars
    _A: ($) => /[aA]/,
    _B: ($) => /[bB]/,
    _C: ($) => /[cC]/,
    _D: ($) => /[dD]/,
    _E: ($) => /[eE]/,
    _F: ($) => /[fF]/,
    _G: ($) => /[gG]/,
    _H: ($) => /[hH]/,
    _I: ($) => /[iI]/,
    _J: ($) => /[jJ]/,
    _K: ($) => /[kK]/,
    _L: ($) => /[lL]/,
    _M: ($) => /[mM]/,
    _N: ($) => /[nN]/,
    _O: ($) => /[oO]/,
    _P: ($) => /[pP]/,
    _Q: ($) => /[qQ]/,
    _R: ($) => /[rR]/,
    _S: ($) => /[sS]/,
    _T: ($) => /[tT]/,
    _U: ($) => /[uU]/,
    _V: ($) => /[vV]/,
    _W: ($) => /[wW]/,
    _X: ($) => /[xX]/,
    _Y: ($) => /[yY]/,
    _Z: ($) => /[zZ]/,
  },
  conflicts: ($) => [
    [$.programUnit],
    [$.dataDivision],
    [$.environmentDivision],
    [$.identificationDivision],
    [$.identifier, $.tableCall],
    [$.qualifiedDataNameFormat1],
    [$.conditionName, $.dataName, $.fileName],
    [$.conditionName, $.dataName],
    [$.conditionName, $.dataName, $.fileName, $.paragraphName, $.textName],
    [$.procedureDivisionUsingClause],
    [$.procedureDivisionByReferencePhrase],
    [$.procedureDivisionBody],
    [$.paragraph],
    [$.exitStatement],
    [$.execSqlStatement],
    [$.paragraphs],
    [$.execCicsStatement],
    [$.execSqlImsStatement],
    [$.specialNamesParagraph],
    [$.conditionName, $.dataName, $.paragraphName, $.textName],
    [$.paragraphName, $.numericLiteral],
    [$.procedureDivisionByValuePhrase],
    [$.paragraph, $.sentence],
    [$.acceptStatement],
    [$.addStatement],
    [$.procedureName],
    [$.alterStatement],
    [$.callStatement],
    [$.cancelStatement],
    [$.closeFile],
    [$.closeStatement],
    [$.computeStore],
    [$.deleteStatement],
    [$.displayStatement],
    [$.entryStatement],
    [$.evaluateStatement],
    [$.condition],
    [$.goToStatementSimple, $.goToDependingOnStatement],
    [$.goToDependingOnStatement],
    [$.initializeStatement],
    [$.initiateStatement],
    [$.openStatement],
    [$.qualifiedDataNameFormat2, $.procedureName],
    [$.performProcedureStatement],
    [$.conditionName, $.dataName, $.paragraphName],
    [$.performTimes, $.paragraphName],
    [$.purgeStatement],
    [$.readStatement],
    [$.receiveStatement],
    [$.rewriteStatement],
    [$.sendStatement],
    [$.sendStatementSync],
    [$.setStatement],
    [$.startStatement],
    [$.stopStatement, $.stopStatementGiving],
    [$.subtractStatement],
    [$.writeStatement],
    [$.remarksParagraph],
    [$.programIdParagraph],
    [$.communicationSection],
    [$.fileSection],
    [$.localStorageSection],
    [$.programLibrarySection],
    [$.reportSection],
    [$.configurationSection],
    [$.inputOutputSection],
    [$.subscript],
    [$.subscript, $.paragraphName],
    [$.tableCall],
    [$.qualifiedDataNameFormat1, $.inData],
    [$.numericLiteral, $.figurativeConstant],
    [$.onExceptionClause],
    [$.onOverflowPhrase],
    [$.closeReelUnitStatement],
    [$.invalidKeyPhrase],
    [$.divideStatement],
    [$.arithmeticExpression],
    [$.evaluateWhenPhrase],
    [$.ifStatement],
    [$.ifThen],
    [$.mergeStatement],
    [$.moveToSendingArea, $.figurativeConstant],
    [$.openExtendStatement],
    [$.openIOStatement],
    [$.openInputStatement],
    [$.openOutput],
    [$.openOutputStatement],
    [$.performVarying],
    [$.atEndPhrase],
    [$.returnStatement],
    [$.searchStatement],
    [$.sortStatement],
    [$.stringStatement],
    [$.unstringStatement],
    [$.writeAtEndOfPagePhrase],
    [$.dataDescriptionEntryExecSql],
    [$.fileControlEntry],
    [$.fileControlParagraph],
    [$.environmentSwitchNameClause],
    [$.currencySignClause],
    [$.defaultDisplaySignClause],
    [$.reserveNetworkClause],
    [$.symbolicCharactersClause],
    [$.qualifiedDataNameFormat4, $.specialRegister],
    [$.argument],
    [$.identifier, $.tableCall, $.argument],
    [$.NUMERICLITERAL],
    [$.acceptFromDateStatement],
    [$.notOnExceptionClause],
    [$.onSizeErrorPhrase],
    [$.addGiving],
    [$.addToGivingStatement],
    [$.addTo, $.addToGiving],
    [$.addToStatement],
    [$.callUsingPhrase],
    [$.callByReferencePhrase],
    [$.closePortFileIOStatement],
    [$.computeStatement],
    [$.notInvalidKeyPhrase],
    [$.mnemonicName, $.systemName],
    [$.divideByGivingStatement],
    [$.divideIntoGivingStatement, $.divideInto],
    [$.divideIntoGivingStatement],
    [$.divideIntoStatement],
    [$.conditionNameReference, $.qualifiedDataNameFormat1],
    [$.evaluateWhen],
    [$.evaluateCondition],
    [$.evaluateWhenOther],
    [$.andOrCondition],
    [$.ifElse],
    [$.ifThen, $.nextSentenceStatement],
    [$.initializeReplacingPhrase],
    [$.inspectReplacingPhrase],
    [$.inspectTallyingPhrase],
    [$.inspectTallyingPhrase, $.inspectTallyingReplacingPhrase],
    [$.mergeOnKeyClause],
    [$.moveToStatement],
    [$.multiplyStatement],
    [$.multiplyRegularOperand, $.multiplyGivingOperand],
    [$.multiplyRegularOperand],
    [$.multiplyRegular],
    [$.performVaryingClause],
    [$.notAtEndPhrase],
    [$.receiveIntoStatement],
    [$.receiveFromStatement],
    [$.searchWhen],
    [$.conditionName, $.dataName, $.mnemonicName],
    [$.conditionName, $.dataName, $.mnemonicName, $.paragraphName, $.textName],
    [$.setToStatement],
    [$.sortOnKeyClause],
    [$.sortDuplicatesPhrase],
    [$.stopStatementGiving, $.paragraphName],
    [$.subtractMinuend, $.subtractMinuendGiving],
    [$.subtractMinuend],
    [$.subtractFromStatement],
    [$.unstringInto],
    [$.unstringIntoPhrase],
    [$.writeNotAtEndOfPagePhrase],
    [$.dataValueClause],
    [$.dataValueInterval],
    [$.dataValueIntervalFrom, $.dataName],
    [$.dataExternalClause],
    [$.dataJustifiedClause],
    [$.dataSignClause],
    [$.dataSynchronizedClause],
    [$.screenDescriptionJustifiedClause],
    [$.screenDescriptionSignClause],
    [$.objectComputerParagraph],
    [$.sourceComputerParagraph],
    [$.organizationClause, $.relativeKeyClause],
    [$.ioControlParagraph],
    [$.alphabetLiterals],
    [$.alphabetClauseFormat1],
    [$.classClauseThrough],
    [$.classClause],
    [$.functionCall],
    [$.notOnSizeErrorPhrase],
    [$.addTo],
    [$.callByContentPhrase],
    [$.callByValuePhrase],
    [$.divideInto],
    [$.conditionNameReference, $.qualifiedInData],
    [$.conditionNameReference],
    [$.relationalOperator],
    [$.inspectConvertingPhrase],
    [$.inspectReplacingAllLeadings],
    [$.inspectReplacingCharacters],
    [$.inspectTallyingReplacingPhrase],
    [$.mergeGiving],
    [$.mergeGivingPhrase],
    [$.mergeCollatingSequencePhrase],
    [$.mergeUsing],
    [$.moveCorrespondingToStatement],
    [$.sortGiving],
    [$.sortGivingPhrase],
    [$.sortCollatingSequencePhrase],
    [$.sortUsing],
    [$.notOnOverflowPhrase],
    [$.subtractMinuendCorresponding],
    [$.unstringDelimitedByPhrase, $.figurativeConstant],
    [$.ENTRY, $.ENTRY_PROCEDURE],
    [$.dataOccursClause],
    [$.dataOccursClause, $.paragraphName],
    [$.pictureString],
    [$.dataUsageClause],
    [$.fileDescriptionEntry],
    [$.libraryDescriptionEntryFormat2],
    [$.libraryDescriptionEntryFormat1],
    [
      $.reportGroupDescriptionEntryFormat2,
      $.reportGroupDescriptionEntryFormat3,
    ],
    [$.reportGroupLineNumberNextPage],
    [$.reportGroupIndicateClause],
    [$.reportGroupJustifiedClause],
    [$.screenDescriptionBackgroundColorClause, $.paragraphName],
    [$.screenDescriptionColumnClause, $.paragraphName],
    [$.screenDescriptionForegroundColorClause, $.paragraphName],
    [$.screenDescriptionFromClause],
    [$.screenDescriptionLineClause, $.paragraphName],
    [$.screenDescriptionPromptClause],
    [$.screenDescriptionSizeClause, $.paragraphName],
    [$.recordKeyClause],
    [$.reserveClause],
    [$.fileStatusClause],
    [$.callByReference, $.specialRegister],
    [$.divideGiving],
    [$.divideGivingPhrase],
    [$.dataName, $.fileName, $.mnemonicName],
    [$.conditionName, $.dataName, $.fileName, $.mnemonicName],
    [
      $.conditionName,
      $.dataName,
      $.fileName,
      $.mnemonicName,
      $.paragraphName,
      $.textName,
    ],
    [$.ifElse, $.nextSentenceStatement],
    [$.inspectReplacingAllLeading],
    [$.inspectCharacters],
    [$.inspectFor],
    [$.mergeOutputProcedurePhrase],
    [$.multiplyGivingResult],
    [$.multiplyGiving],
    [$.receiveNoData],
    [$.receiveWithData],
    [$.nextSentenceStatement, $.searchWhen],
    [$.sortInputProcedurePhrase],
    [$.sortOutputProcedurePhrase],
    [$.subtractGiving],
    [$.subtractFromGivingStatement],
    [$.unstringOrAllPhrase, $.figurativeConstant],
    [$.pictureChars, $.pictureCardinality],
    [$.dataUsingClause, $.dataName],
    [$.blockContainsClause],
    [$.linageClause],
    [$.recordContainsClauseFormat1],
    [$.recordContainsClauseFormat2],
    [$.reportClause],
    [$.libraryAttributeClauseFormat2],
    [$.libraryAttributeClauseFormat1],
    [
      $.reportGroupDescriptionEntryFormat1,
      $.reportGroupDescriptionEntryFormat3,
    ],
    [$.reportGroupSumClause],
    [$.reportDescriptionPageLimitClause],
    [$.INTEGERLITERAL],
    [$.alternateRecordKeyClause],
    [$.sameClause],
    [$.environmentSwitchNameSpecialNamesStatusPhrase],
    [$.alphabetAlso],
    [$.callByContent, $.specialRegister],
    [$.callByValue, $.specialRegister],
    [$.closePortFileIOUsingAssociatedData, $.paragraphName],
    [$.closePortFileIOUsingAssociatedDataLength, $.paragraphName],
    [$.dataName, $.fileName],
    [$.inspectAllLeading],
    [$.inspectAllLeadings],
    [$.dataOccursSort],
    [$.dataOccursIndexed],
    [$.dataRecordsClause],
    [$.labelRecordsClause],
    [$.linageClause, $.linageLinesAtBottom],
    [$.recordContainsClauseFormat3],
    [$.valueOfClause],
    [$.libraryEntryProcedureClauseFormat2],
    [$.libraryEntryProcedureClauseFormat1],
    [$.reportGroupSignClause],
    [$.screenDescriptionPromptOccursClause],
    [$.diskSizeClause],
    [$.memorySizeClause],
    [$.collatingSequenceClause],
    [$.multipleFilePosition],
    [$.multipleFileClause],
    [$.rerunEveryClock],
    [$.rerunEveryRecords, $.rerunEveryClock],
    [$.fileName, $.systemName],
    [$.conditionName, $.dataName, $.fileName, $.paragraphName],
    [$.linageLinesAtBottom],
    [$.linageLinesAtTop],
    [$.libraryEntryProcedureUsingClause],
    [$.fileName, $.localName],
    [$.libraryEntryProcedureWithClause],
    [$.destinationTableClause],
    [$.defaultComputationalSignClause],
    [ $.symbolicCharactersClause, $.cobolWord],
[ $.callByReference, $.cobolWord],
[ $.dataUsageClause, $.cobolWord],
[ $.dataIntegerStringClause, $.cobolWord],
[ $.dataCommonOwnLocalClause, $.cobolWord],
[ $.dataWithLowerBoundsClause, $.cobolWord],
[ $.dataReceivedByClause, $.cobolWord],
[ $.dataThreadLocalClause, $.cobolWord],
[ $.dataTypeDefClause, $.cobolWord],
[ $.screenDescriptionAutoClause, $.cobolWord],
[ $.screenDescriptionBackgroundColorClause, $.cobolWord],
[ $.screenDescriptionBellClause, $.cobolWord],
[ $.screenDescriptionBlinkClause, $.cobolWord],
[ $.screenDescriptionRequiredClause, $.cobolWord],
[ $.screenDescriptionEraseClause, $.cobolWord],
[ $.screenDescriptionForegroundColorClause, $.cobolWord],
[ $.screenDescriptionFullClause, $.cobolWord],
[ $.screenDescriptionGridClause, $.cobolWord],
[ $.screenDescriptionLightClause, $.cobolWord],
[ $.screenDescriptionSecureClause, $.cobolWord],
[ $.screenDescriptionPromptClause, $.cobolWord],
[ $.screenDescriptionReverseVideoClause, $.cobolWord],
[ $.screenDescriptionUnderlineClause, $.cobolWord],
[ $.screenDescriptionZeroFillClause, $.cobolWord],
[ $.alphabetClauseFormat1, $.cobolWord],
[ $.alphabetClauseFormat2, $.cobolWord],
[ $.classClause, $.cobolWord],
[ $.assignClause, $.cobolWord],
[ $.organizationClause, $.cobolWord],
[ $.mergeCollatingNational, $.cobolWord],
[ $.sortCollatingNational, $.cobolWord],
[ $.dataDescName, $.cobolWord],
[ $.recordDelimiterClause, $.cobolWord],
[ $.dataOccursIndexed, $.cobolWord],
//HERE
  ],
});
