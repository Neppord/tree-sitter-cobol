/* global module, optional, choice, seq, grammar, field, repeat */
module.exports = grammar({
  name: "Cobol",
  rules: {
    startRule: ($) => seq(compilationUnit, EOF),
    compilationUnit: ($) => repeat(programUnit),
    programUnit: ($) =>
      seq(
        identificationDivision,
        optional(environmentDivision),
        optional(dataDivision),
        optional(procedureDivision),
        repeat(programUnit),
        optional(endProgramStatement)
      ),
    endProgramStatement: ($) => seq(END, PROGRAM, programName, DOT_FS),
    // --- identification division --------------------------------------------------------------------
    identificationDivision: ($) =>
      seq(
        choice(IDENTIFICATION, ID),
        DIVISION,
        DOT_FS,
        programIdParagraph,
        repeat(identificationDivisionBody)
      ),
    identificationDivisionBody: ($) =>
      choice(
        authorParagraph,
        installationParagraph,
        dateWrittenParagraph,
        dateCompiledParagraph,
        securityParagraph,
        remarksParagraph
      ),
    // - program id paragraph ----------------------------------
    programIdParagraph: ($) =>
      seq(
        PROGRAM_ID,
        DOT_FS,
        programName,
        optional(
          seq(
            optional(IS),
            choice(COMMON, INITIAL, LIBRARY, DEFINITION, RECURSIVE),
            optional(PROGRAM)
          )
        ),
        optional(DOT_FS),
        optional(commentEntry)
      ),
    // - author paragraph ----------------------------------
    authorParagraph: ($) => seq(AUTHOR, DOT_FS, optional(commentEntry)),
    // - installation paragraph ----------------------------------
    installationParagraph: ($) =>
      seq(INSTALLATION, DOT_FS, optional(commentEntry)),
    // - date written paragraph ----------------------------------
    dateWrittenParagraph: ($) =>
      seq(DATE_WRITTEN, DOT_FS, optional(commentEntry)),
    // - date compiled paragraph ----------------------------------
    dateCompiledParagraph: ($) =>
      seq(DATE_COMPILED, DOT_FS, optional(commentEntry)),
    // - security paragraph ----------------------------------
    securityParagraph: ($) => seq(SECURITY, DOT_FS, optional(commentEntry)),
    // - remarks paragraph ----------------------------------
    remarksParagraph: ($) =>
      seq(
        REMARKS,
        DOT_FS,
        optional(commentEntry),
        optional(END_REMARKS),
        optional(DOT_FS)
      ),
    // --- environment division --------------------------------------------------------------------
    environmentDivision: ($) =>
      seq(ENVIRONMENT, DIVISION, DOT_FS, repeat(environmentDivisionBody)),
    environmentDivisionBody: ($) =>
      choice(configurationSection, specialNamesParagraph, inputOutputSection),
    // -- configuration section ----------------------------------
    configurationSection: ($) =>
      seq(
        CONFIGURATION,
        SECTION,
        DOT_FS,
        repeat(configurationSectionParagraph)
      ),
    // - configuration section paragraph ----------------------------------
    configurationSectionParagraph: ($) =>
      choice(
        sourceComputerParagraph,
        objectComputerParagraph,
        specialNamesParagraph
      ),
    // strictly, specialNamesParagraph does not belong into configurationSectionParagraph, but IBM-COBOL allows this,
    // - source computer paragraph ----------------------------------
    sourceComputerParagraph: ($) =>
      seq(
        SOURCE_COMPUTER,
        DOT_FS,
        optional(
          seq(
            computerName,
            optional(seq(optional(WITH), DEBUGGING, MODE)),
            DOT_FS
          )
        )
      ),
    // - object computer paragraph ----------------------------------
    objectComputerParagraph: ($) =>
      seq(
        OBJECT_COMPUTER,
        DOT_FS,
        optional(seq(computerName, repeat(objectComputerClause), DOT_FS))
      ),
    objectComputerClause: ($) =>
      choice(
        memorySizeClause,
        diskSizeClause,
        collatingSequenceClause,
        segmentLimitClause,
        characterSetClause
      ),
    memorySizeClause: ($) =>
      seq(
        MEMORY,
        optional(SIZE),
        choice(integerLiteral, cobolWord),
        optional(choice(WORDS, CHARACTERS, MODULES))
      ),
    diskSizeClause: ($) =>
      seq(
        DISK,
        optional(SIZE),
        optional(IS),
        choice(integerLiteral, cobolWord),
        optional(choice(WORDS, MODULES))
      ),
    collatingSequenceClause: ($) =>
      seq(
        optional(PROGRAM),
        optional(COLLATING),
        SEQUENCE,
        seq(optional(IS), repeat1(alphabetName)),
        optional(collatingSequenceClauseAlphanumeric),
        optional(collatingSequenceClauseNational)
      ),
    collatingSequenceClauseAlphanumeric: ($) =>
      seq(optional(FOR), ALPHANUMERIC, optional(IS), alphabetName),
    collatingSequenceClauseNational: ($) =>
      seq(optional(FOR), NATIONAL, optional(IS), alphabetName),
    segmentLimitClause: ($) => seq(SEGMENT_LIMIT, optional(IS), integerLiteral),
    characterSetClause: ($) => seq(CHARACTER, SET, DOT_FS),
    // - special names paragraph ----------------------------------
    specialNamesParagraph: ($) =>
      seq(
        SPECIAL_NAMES,
        DOT_FS,
        optional(seq(repeat1(specialNameClause), DOT_FS))
      ),
    specialNameClause: ($) =>
      choice(
        channelClause,
        odtClause,
        alphabetClause,
        classClause,
        currencySignClause,
        decimalPointClause,
        symbolicCharactersClause,
        environmentSwitchNameClause,
        defaultDisplaySignClause,
        defaultComputationalSignClause,
        reserveNetworkClause
      ),
    alphabetClause: ($) => choice(alphabetClauseFormat1, alphabetClauseFormat2),
    alphabetClauseFormat1: ($) =>
      seq(
        ALPHABET,
        alphabetName,
        optional(seq(FOR, ALPHANUMERIC)),
        optional(IS),
        seq(
          choice(
            EBCDIC,
            ASCII,
            STANDARD_1,
            STANDARD_2,
            NATIVE,
            cobolWord,
            repeat1(alphabetLiterals)
          )
        )
      ),
    alphabetLiterals: ($) =>
      seq(literal, optional(choice(alphabetThrough, repeat1(alphabetAlso)))),
    alphabetThrough: ($) => seq(choice(THROUGH, THRU), literal),
    alphabetAlso: ($) => seq(ALSO, repeat1(literal)),
    alphabetClauseFormat2: ($) =>
      seq(
        ALPHABET,
        alphabetName,
        optional(FOR),
        NATIONAL,
        optional(IS),
        seq(choice(NATIVE, CCSVERSION), literal)
      ),
    channelClause: ($) =>
      seq(CHANNEL, integerLiteral, optional(IS), mnemonicName),
    classClause: ($) =>
      seq(
        CLASS,
        className,
        otional(seq(optional(FOR), choice(ALPHANUMERIC, NATIONAL))),
        optional(IS),
        repeat1(classClauseThrough)
      ),
    classClauseThrough: ($) =>
      seq(classClauseFrom, optional(seq(choice(THROUGH, THRU), classClauseTo))),
    classClauseFrom: ($) => choice(identifier, literal),
    classClauseTo: ($) => choice(identifier, literal),
    currencySignClause: ($) =>
      seq(
        CURRENCY,
        optional(SIGN),
        optional(IS),
        literal,
        optional(seq(optional(WITH), PICTURE, SYMBOL, literal))
      ),
    decimalPointClause: ($) => seq(DECIMAL_POINT, optional(IS), COMMA),
    defaultComputationalSignClause: ($) =>
      seq(
        DEFAULT,
        optional(choice(COMPUTATIONAL, COMP)),
        optional(seq(SIGN, optional(IS))),
        optional(choice(LEADING, TRAILING)),
        seq(SEPARATE, optional(CHARACTER))
      ),
    defaultDisplaySignClause: ($) =>
      seq(
        DEFAULT_DISPLAY,
        optional(seq(SIGN, optional(IS))),
        choice(LEADING, TRAILING),
        optional(seq(SEPARATE, optional(CHARACTER)))
      ),
    environmentSwitchNameClause: ($) =>
      seq(
        environmentName,
        optional(IS),
        mnemonicName,
        choice(
          optional(environmentSwitchNameSpecialNamesStatusPhrase),
          environmentSwitchNameSpecialNamesStatusPhrase
        )
      ),
    environmentSwitchNameSpecialNamesStatusPhrase: ($) =>
      seq(
        ON,
        optional(STATUS),
        optional(IS),
        condition,
        choice(
          optional(seq(OFF, optional(STATUS), optional(IS), condition)),
          OFF
        ),
        optional(STATUS),
        optional(IS),
        condition,
        optional(seq(ON, optional(STATUS), optional(IS), condition))
      ),
    odtClause: ($) => seq(ODT, optional(IS), mnemonicName),
    reserveNetworkClause: ($) =>
      seq(
        RESERVE,
        optional(WORDS),
        optional(LIST),
        optional(IS),
        NETWORK,
        optional(CAPABLE)
      ),
    symbolicCharactersClause: ($) =>
      seq(
        SYMBOLIC,
        optional(CHARACTERS),
        optional(seq(optional(FOR), choice(ALPHANUMERIC, NATIONAL))),
        repeat1(symbolicCharacters),
        optional(seq(IN, alphabetName))
      ),
    symbolicCharacters: ($) =>
      seq(
        repeat1(symbolicCharacter),
        optional(choice(IS, ARE)),
        repeat1(integerLiteral)
      ),
    // -- input output section ----------------------------------
    inputOutputSection: ($) =>
      seq(INPUT_OUTPUT, SECTION, DOT_FS, repeat(inputOutputSectionParagraph)),
    // - input output section paragraph ----------------------------------
    inputOutputSectionParagraph: ($) =>
      choice(fileControlParagraph, ioControlParagraph),
    // - file control paragraph ----------------------------------
    fileControlParagraph: ($) =>
      seq(
        optional(FILE_CONTROL),
        repeat(seq(optional(DOT_FS), fileControlEntry)),
        DOT_FS
      ),
    fileControlEntry: ($) => seq(selectClause, repeat(fileControlClause)),
    selectClause: ($) => seq(SELECT, optional(OPTIONAL), fileName),
    fileControlClause: ($) =>
      choice(
        assignClause,
        reserveClause,
        organizationClause,
        paddingCharacterClause,
        recordDelimiterClause,
        accessModeClause,
        recordKeyClause,
        alternateRecordKeyClause,
        fileStatusClause,
        passwordClause,
        relativeKeyClause
      ),
    assignClause: ($) =>
      seq(
        ASSIGN,
        optional(TO),
        choice(
          DISK,
          DISPLAY,
          KEYBOARD,
          PORT,
          PRINTER,
          READER,
          REMOTE,
          TAPE,
          VIRTUAL,
          seq(optional(choice(DYNAMIC, EXTERNAL)), assignmentName),
          literal
        )
      ),
    reserveClause: ($) =>
      seq(
        RESERVE,
        choice(NO, integerLiteral),
        optional(ALTERNATE),
        optional(choice(AREA, AREAS))
      ),
    organizationClause: ($) =>
      seq(
        optional(seq(ORGANIZATION, optional(IS))),
        optional(seq(choice(LINE, RECORD), choice(BINARY, RECORD, BINARY))),
        choice(SEQUENTIAL, RELATIVE, INDEXED)
      ),
    paddingCharacterClause: ($) =>
      seq(
        PADDING,
        optional(CHARACTER),
        optional(IS),
        choice(qualifiedDataName, literal)
      ),
    recordDelimiterClause: ($) =>
      seq(
        RECORD,
        DELIMITER,
        optional(IS),
        choice(STANDARD_1, IMPLICIT, assignmentName)
      ),
    accessModeClause: ($) =>
      seq(
        ACCESS,
        optional(MODE),
        optional(IS),
        sechoice(SEQUENTIAL, RANDOM, DYNAMIC, EXCLUSIVE)
      ),
    recordKeyClause: ($) =>
      seq(
        RECORD,
        optional(KEY),
        optional(IS),
        qualifiedDataName,
        optional(passwordClause),
        optional(seq(optional(WITH), DUPLICATES))
      ),
    alternateRecordKeyClause: ($) =>
      seq(
        ALTERNATE,
        RECORD,
        optional(KEY),
        optional(IS),
        qualifiedDataName,
        optional(passwordClause),
        optional(seq(optional(WITH), DUPLICATES))
      ),
    passwordClause: ($) => seq(PASSWORD, optional(IS), dataName),
    fileStatusClause: ($) =>
      seq(
        optional(FILE),
        STATUS,
        optional(IS),
        qualifiedDataName,
        optional(qualifiedDataName)
      ),
    relativeKeyClause: ($) =>
      seq(RELATIVE, optional(KEY), optional(IS), qualifiedDataName),
    // - io control paragraph ----------------------------------
    ioControlParagraph: ($) =>
      seq(
        I_O_CONTROL,
        DOT_FS,
        optional(seq(fileName, DOT_FS)),
        optional(seq(repeat(ioControlClause), DOT_FS))
      ),
    ioControlClause: ($) =>
      choice(
        rerunClause,
        sameClause,
        multipleFileClause,
        commitmentControlClause
      ),
    rerunClause: ($) =>
      seq(
        RERUN,
        optional(seq(ON, choice(assignmentName, fileName))),
        EVERY,
        choice(rerunEveryRecords, rerunEveryOf, rerunEveryClock)
      ),
    rerunEveryRecords: ($) => seq(integerLiteral, RECORDS),
    rerunEveryOf: ($) =>
      seq(optional(END), optional(OF), choice(REEL, UNIT), OF, fileName),
    rerunEveryClock: ($) => seq(integerLiteral, optional(CLOCK_UNITS)),
    sameClause: ($) =>
      seq(
        SAME,
        optional(choice(RECORD, SORT, SORT_MERGE)),
        optional(AREA),
        optional(FOR),
        repeat1(fileName)
      ),
    multipleFileClause: ($) =>
      seq(
        MULTIPLE,
        FILE,
        optional(TAPE),
        optional(CONTAINS),
        repeat1(multipleFilePosition)
      ),
    multipleFilePosition: ($) =>
      seq(fileName, optional(seq(POSITION, integerLiteral))),
    commitmentControlClause: ($) =>
      seq(COMMITMENT, CONTROL, optional(FOR), fileName),
    // --- data division --------------------------------------------------------------------
    dataDivision: ($) =>
      seq(DATA, DIVISION, DOT_FS, repeat(dataDivisionSection)),
    dataDivisionSection: ($) =>
      choice(
        fileSection,
        dataBaseSection,
        workingStorageSection,
        linkageSection,
        communicationSection,
        localStorageSection,
        screenSection,
        reportSection,
        programLibrarySection
      ),
    // -- file section ----------------------------------
    fileSection: ($) =>
      seq(FILE, SECTION, DOT_FS, repeat(fileDescriptionEntry)),
    fileDescriptionEntry: ($) =>
      seq(
        choice(FD, SD),
        fileName,
        repeat(seq(optional(DOT_FS), fileDescriptionEntryClause)),
        DOT_FS,
        repeat(dataDescriptionEntry)
      ),
    fileDescriptionEntryClause: ($) =>
      choice(
        externalClause,
        globalClause,
        blockContainsClause,
        recordContainsClause,
        labelRecordsClause,
        valueOfClause,
        dataRecordsClause,
        linageClause,
        codeSetClause,
        reportClause,
        recordingModeClause
      ),
    externalClause: ($) => seq(optional(IS), EXTERNAL),
    globalClause: ($) => seq(optional(IS), GLOBAL),
    blockContainsClause: ($) =>
      seq(
        BLOCK,
        optional(CONTAINS),
        integerLiteral,
        optional(blockContainsTo),
        optional(choice(RECORDS, CHARACTERS))
      ),
    blockContainsTo: ($) => seq(TO, integerLiteral),
    recordContainsClause: ($) =>
      seq(
        RECORD,
        choice(
          recordContainsClauseFormat1,
          recordContainsClauseFormat2,
          recordContainsClauseFormat3
        )
      ),
    recordContainsClauseFormat1: ($) =>
      seq(optional(CONTAINS), integerLiteral, optional(CHARACTERS)),
    recordContainsClauseFormat2: ($) =>
      seq(
        optional(IS),
        VARYING,
        optional(IN),
        optional(SIZE),
        optional(
          seq(
            optional(FROM),
            integerLiteral,
            optional(recordContainsTo),
            optional(CHARACTERS)
          )
        ),
        optional(seq(DEPENDING, optional(ON), qualifiedDataName))
      ),
    recordContainsClauseFormat3: ($) =>
      seq(
        optional(CONTAINS),
        integerLiteral,
        recordContainsTo,
        optional(CHARACTERS)
      ),
    recordContainsTo: ($) => seq(TO, integerLiteral),
    labelRecordsClause: ($) =>
      seq(
        LABEL,
        choice(seq(RECORD, optional(IS)), seq(RECORDS, optional(ARE))),
        choice(OMITTED, STANDARD, repeat1(dataName))
      ),
    valueOfClause: ($) => seq(VALUE, OF, repeat1(valuePair)),
    valuePair: ($) =>
      seq(systemName, optional(IS), choice(qualifiedDataName, literal)),
    dataRecordsClause: ($) =>
      seq(
        DATA,
        choice(seq(RECORD, optional(IS)), seq(RECORDS, optional(ARE))),
        repeat1(dataName)
      ),
    linageClause: ($) =>
      seq(
        LINAGE,
        optional(IS),
        choice(dataName, integerLiteral),
        optional(LINES),
        repeat(linageAt)
      ),
    linageAt: ($) =>
      choice(linageFootingAt, linageLinesAtTop, linageLinesAtBottom),
    linageFootingAt: ($) =>
      seq(
        optional(WITH),
        FOOTING,
        optional(AT),
        choice(dataName, integerLiteral)
      ),
    linageLinesAtTop: ($) =>
      seq(optional(LINES), optional(AT), TOP, choice(dataName, integerLiteral)),
    linageLinesAtBottom: ($) =>
      seq(
        optional(LINES),
        optional(AT),
        BOTTOM,
        choice(dataName, integerLiteral)
      ),
    recordingModeClause: ($) =>
      seq(RECORDING, optional(MODE), optional(IS), modeStatement),
    modeStatement: ($) => cobolWord,
    codeSetClause: ($) => seq(CODE_SET, optional(IS), alphabetName),
    reportClause: ($) =>
      seq(
        choice(seq(REPORT, optional(IS)), seq(REPORTS, optional(ARE))),
        repeat1(reportName)
      ),
    // -- data base section ----------------------------------
    dataBaseSection: ($) =>
      seq(DATA_BASE, SECTION, DOT_FS, repeat(dataBaseSectionEntry)),
    dataBaseSectionEntry: ($) => seq(integerLiteral, literal, INVOKE, literal),
    // -- working storage section ----------------------------------
    workingStorageSection: ($) =>
      seq(WORKING_STORAGE, SECTION, DOT_FS, repeat(dataDescriptionEntry)),
    // -- linkage section ----------------------------------
    linkageSection: ($) =>
      seq(LINKAGE, SECTION, DOT_FS, repeat(dataDescriptionEntry)),
    // -- communication section ----------------------------------
    communicationSection: ($) =>
      seq(
        COMMUNICATION,
        SECTION,
        DOT_FS,
        repeat(choice(communicationDescriptionEntry, dataDescriptionEntry))
      ),
    communicationDescriptionEntry: ($) =>
      choice(
        communicationDescriptionEntryFormat1,
        communicationDescriptionEntryFormat2,
        communicationDescriptionEntryFormat3
      ),
    communicationDescriptionEntryFormat1: ($) =>
      seq(
        CD,
        cdName,
        optional(FOR),
        optional(INITIAL),
        INPUT,
        repeat(
          choice(
            symbolicQueueClause,
            symbolicSubQueueClause,
            messageDateClause,
            messageTimeClause,
            symbolicSourceClause,
            textLengthClause,
            endKeyClause,
            statusKeyClause,
            messageCountClause,
            dataDescName
          )
        ),
        DOT_FS
      ),
    communicationDescriptionEntryFormat2: ($) =>
      seq(
        CD,
        cdName,
        optional(FOR),
        OUTPUT,
        repeat(
          choice(
            destinationCountClause,
            textLengthClause,
            statusKeyClause,
            destinationTableClause,
            errorKeyClause,
            symbolicDestinationClause
          )
        ),
        DOT_FS
      ),
    communicationDescriptionEntryFormat3: ($) =>
      seq(
        CD,
        cdName,
        optional(FOR),
        INITIAL,
        I_O,
        repeat(
          choice(
            messageDateClause,
            messageTimeClause,
            symbolicTerminalClause,
            textLengthClause,
            endKeyClause,
            statusKeyClause,
            dataDescName
          )
        ),
        DOT_FS
      ),
    destinationCountClause: ($) =>
      seq(DESTINATION, COUNT, optional(IS), dataDescName),
    destinationTableClause: ($) =>
      seq(
        DESTINATION,
        TABLE,
        OCCURS,
        integerLiteral,
        TIMES,
        optional(seq(INDEXED, BY, repeat1(indexName)))
      ),
    endKeyClause: ($) => seq(END, KEY, optional(IS), dataDescName),
    errorKeyClause: ($) => seq(ERROR, KEY, optional(IS), dataDescName),
    messageCountClause: ($) =>
      seq(optional(MESSAGE), COUNT, optional(IS), dataDescName),
    messageDateClause: ($) => seq(MESSAGE, DATE, optional(IS), dataDescName),
    messageTimeClause: ($) => seq(MESSAGE, TIME, optional(IS), dataDescName),
    statusKeyClause: ($) => seq(STATUS, KEY, optional(IS), dataDescName),
    symbolicDestinationClause: ($) =>
      seq(optional(SYMBOLIC), DESTINATION, optional(IS), dataDescName),
    symbolicQueueClause: ($) =>
      seq(optional(SYMBOLIC), QUEUE, optional(IS), dataDescName),
    symbolicSourceClause: ($) =>
      seq(optional(SYMBOLIC), SOURCE, optional(IS), dataDescName),
    symbolicTerminalClause: ($) =>
      seq(optional(SYMBOLIC), TERMINAL, optional(IS), dataDescName),
    symbolicSubQueueClause: ($) =>
      seq(
        optional(SYMBOLIC),
        choice(SUB_QUEUE_1, SUB_QUEUE_2, SUB_QUEUE_3),
        optional(IS),
        dataDescName
      ),
    textLengthClause: ($) => seq(TEXT, LENGTH, optional(IS), dataDescName),
    // -- local storage section ----------------------------------
    localStorageSection: ($) =>
      seq(
        LOCAL_STORAGE,
        SECTION,
        DOT_FS,
        optional(seq(LD, localName, DOT_FS)),
        repeat(dataDescriptionEntry)
      ),
    // -- screen section ----------------------------------
    screenSection: ($) =>
      seq(SCREEN, SECTION, DOT_FS, repeat(screenDescriptionEntry)),
    screenDescriptionEntry: ($) =>
      seq(
        INTEGERLITERAL,
        optional(choice(FILLER, screenName)),
        repeat(
          choice(
            screenDescriptionBlankClause,
            screenDescriptionBellClause,
            screenDescriptionBlinkClause,
            screenDescriptionEraseClause,
            screenDescriptionLightClause,
            screenDescriptionGridClause,
            screenDescriptionReverseVideoClause,
            screenDescriptionUnderlineClause,
            screenDescriptionSizeClause,
            screenDescriptionLineClause,
            screenDescriptionColumnClause,
            screenDescriptionForegroundColorClause,
            screenDescriptionBackgroundColorClause,
            screenDescriptionControlClause,
            screenDescriptionValueClause,
            screenDescriptionPictureClause,
            screenDescriptionFromClause,
            screenDescriptionUsingClause,
            screenDescriptionUsageClause,
            screenDescriptionBlankWhenZeroClause,
            screenDescriptionJustifiedClause,
            screenDescriptionSignClause,
            screenDescriptionAutoClause,
            screenDescriptionSecureClause,
            screenDescriptionRequiredClause,
            screenDescriptionPromptClause,
            screenDescriptionFullClause,
            screenDescriptionZeroFillClause
          )
        ),
        DOT_FS
      ),
    screenDescriptionBlankClause: ($) => seq(BLANK, choice(SCREEN, LINE)),
    screenDescriptionBellClause: ($) => choice(BELL, BEEP),
    screenDescriptionBlinkClause: ($) => BLINK,
    screenDescriptionEraseClause: ($) => seq(ERASE, choice(EOL, EOS)),
    screenDescriptionLightClause: ($) => choice(HIGHLIGHT, LOWLIGHT),
    screenDescriptionGridClause: ($) => choice(GRID, LEFTLINE, OVERLINE),
    screenDescriptionReverseVideoClause: ($) => REVERSE_VIDEO,
    screenDescriptionUnderlineClause: ($) => UNDERLINE,
    screenDescriptionSizeClause: ($) =>
      seq(SIZE, optional(IS), choice(identifier, integerLiteral)),
    screenDescriptionLineClause: ($) =>
      seq(
        LINE,
        optional(
          seq(optional(NUMBER), optional(IS), choice(PLUS, PLUSCHAR, MINUSCHAR))
        ),
        choice(identifier, integerLiteral)
      ),
    screenDescriptionColumnClause: ($) =>
      seq(
        choice(COLUMN, COL),
        optional(
          seq(optional(NUMBER), optional(IS), choice(PLUS, PLUSCHAR, MINUSCHAR))
        ),
        choice(identifier, integerLiteral)
      ),
    screenDescriptionForegroundColorClause: ($) =>
      seq(
        choice(FOREGROUND_COLOR, FOREGROUND_COLOUR),
        optional(IS),
        choice(identifier, integerLiteral)
      ),
    screenDescriptionBackgroundColorClause: ($) =>
      seq(
        choice(BACKGROUND_COLOR, BACKGROUND_COLOUR),
        optional(IS),
        choice(identifier, integerLiteral)
      ),
    screenDescriptionControlClause: ($) =>
      seq(CONTROL, optional(IS), identifier),
    screenDescriptionValueClause: ($) => seq(VALUE, optional(IS), literal),
    screenDescriptionPictureClause: ($) =>
      seq(choice(PICTURE, PIC), optional(IS), pictureString),
    screenDescriptionFromClause: ($) =>
      seq(
        FROM,
        choice(identifier, literal),
        optional(screenDescriptionToClause)
      ),
    screenDescriptionToClause: ($) => seq(TO, identifier),
    screenDescriptionUsingClause: ($) => seq(USING, identifier),
    screenDescriptionUsageClause: ($) =>
      seq(USAGE, optional(IS), choice(DISPLAY, DISPLAY_1)),
    screenDescriptionBlankWhenZeroClause: ($) =>
      seq(BLANK, optional(WHEN), ZERO),
    screenDescriptionJustifiedClause: ($) =>
      seq(choice(JUSTIFIED, JUST), optional(RIGHT)),
    screenDescriptionSignClause: ($) =>
      seq(
        optional(seq(SIGN, optional(IS))),
        choice(LEADING, TRAILING),
        optional(seq(SEPARATE, optional(CHARACTER)))
      ),
    screenDescriptionAutoClause: ($) => choice(AUTO, AUTO_SKIP),
    screenDescriptionSecureClause: ($) => choice(SECURE, NO_ECHO),
    screenDescriptionRequiredClause: ($) => choice(REQUIRED, EMPTY_CHECK),
    screenDescriptionPromptClause: ($) =>
      seq(
        PROMPT,
        optional(CHARACTER),
        optional(IS),
        choice(identifier, literal),
        optional(screenDescriptionPromptOccursClause)
      ),
    screenDescriptionPromptOccursClause: ($) =>
      seq(OCCURS, integerLiteral, optional(TIMES)),
    screenDescriptionFullClause: ($) => choice(FULL, LENGTH_CHECK),
    screenDescriptionZeroFillClause: ($) => ZERO_FILL,
    // -- report section ----------------------------------
    reportSection: ($) =>
      seq(REPORT, SECTION, DOT_FS, repeat(reportDescription)),
    reportDescription: ($) =>
      seq(reportDescriptionEntry, repeat1(reportGroupDescriptionEntry)),
    reportDescriptionEntry: ($) =>
      seq(
        RD,
        reportName,
        optional(reportDescriptionGlobalClause),
        optional(
          seq(
            reportDescriptionPageLimitClause,
            optional(reportDescriptionHeadingClause),
            optional(reportDescriptionFirstDetailClause),
            optional(reportDescriptionLastDetailClause),
            optional(reportDescriptionFootingClause)
          )
        ),
        DOT_FS
      ),
    reportDescriptionGlobalClause: ($) => seq(optional(IS), GLOBAL),
    reportDescriptionPageLimitClause: ($) =>
      seq(
        PAGE,
        optional(seq(LIMIT, choice(optional(IS), LIMITS), optional(ARE))),
        integerLiteral,
        optional(choice(LINE, LINES))
      ),
    reportDescriptionHeadingClause: ($) => seq(HEADING, integerLiteral),
    reportDescriptionFirstDetailClause: ($) =>
      seq(FIRST, DETAIL, integerLiteral),
    reportDescriptionLastDetailClause: ($) => seq(LAST, DETAIL, integerLiteral),
    reportDescriptionFootingClause: ($) => sqe(FOOTING, integerLiteral),
    reportGroupDescriptionEntry: ($) =>
      choice(
        reportGroupDescriptionEntryFormat1,
        reportGroupDescriptionEntryFormat2,
        reportGroupDescriptionEntryFormat3
      ),
    reportGroupDescriptionEntryFormat1: ($) =>
      seq(
        integerLiteral,
        dataName,
        optional(reportGroupLineNumberClause),
        optional(reportGroupNextGroupClause),
        reportGroupTypeClause,
        optional(reportGroupUsageClause),
        DOT_FS
      ),
    reportGroupDescriptionEntryFormat2: ($) =>
      seq(
        integerLiteral,
        optional(dataName),
        optional(reportGroupLineNumberClause),
        reportGroupUsageClause,
        DOT_FS
      ),
    reportGroupDescriptionEntryFormat3: ($) =>
      seq(
        integerLiteral,
        optional(dataName),
        repeat(
          choice(
            reportGroupPictureClause,
            reportGroupUsageClause,
            reportGroupSignClause,
            reportGroupJustifiedClause,
            reportGroupBlankWhenZeroClause,
            reportGroupLineNumberClause,
            reportGroupColumnNumberClause,
            reportGroupSourceClause,
            reportGroupValueClause,
            reportGroupSumClause,
            reportGroupResetClause,
            reportGroupIndicateClause
          )
        ),
        DOT_FS
      ),
    reportGroupBlankWhenZeroClause: ($) => seq(BLANK, optional(WHEN), ZERO),
    reportGroupColumnNumberClause: ($) =>
      seq(COLUMN, optional(NUMBER), optional(IS), integerLiteral),
    reportGroupIndicateClause: ($) => seq(GROUP, optional(INDICATE)),
    reportGroupJustifiedClause: ($) =>
      seq(choice(JUSTIFIED, JUST), optional(RIGHT)),
    reportGroupLineNumberClause: ($) =>
      seq(
        optional(LINE),
        optional(NUMBER),
        optional(IS),
        choice(reportGroupLineNumberNextPage, reportGroupLineNumberPlus)
      ),
    reportGroupLineNumberNextPage: ($) =>
      seq(integerLiteral, optional(seq(optional(ON), NEXT, PAGE))),
    reportGroupLineNumberPlus: ($) => seq(PLUS, integerLiteral),
    reportGroupNextGroupClause: ($) =>
      seq(
        NEXT,
        GROUP,
        optional(IS),
        choice(
          integerLiteral,
          reportGroupNextGroupNextPage,
          reportGroupNextGroupPlus
        )
      ),
    reportGroupNextGroupPlus: ($) => seq(PLUS, integerLiteral),
    reportGroupNextGroupNextPage: ($) => seq(NEXT, PAGE),
    reportGroupPictureClause: ($) =>
      seq(choice(PICTURE, PIC), optional(IS), pictureString),
    reportGroupResetClause: ($) =>
      seq(RESET, optional(ON), choice(FINAL, dataName)),
    reportGroupSignClause: ($) =>
      seq(
        SIGN,
        optional(IS),
        choice(LEADING, TRAILING),
        SEPARATE,
        optional(CHARACTER)
      ),
    reportGroupSourceClause: ($) => seq(SOURCE, optional(IS), identifier),
    reportGroupSumClause: ($) =>
      seq(
        SUM,
        identifier,
        repeat(seq(optional(COMMACHAR), identifier)),
        optional(
          seq(UPON, dataName, repeat(seq(optional(COMMACHAR), dataName)))
        )
      ),
    reportGroupTypeClause: ($) =>
      seq(
        TYPE,
        optional(IS),
        choice(
          reportGroupTypeReportHeading,
          reportGroupTypePageHeading,
          reportGroupTypeControlHeading,
          reportGroupTypeDetail,
          reportGroupTypeControlFooting,
          reportGroupTypePageFooting,
          reportGroupTypeReportFooting
        )
      ),
    reportGroupTypeReportHeading: ($) => choice(seq(REPORT, HEADING), RH),
    reportGroupTypePageHeading: ($) => choice(seq(PAGE, HEADING), PH),
    reportGroupTypeControlHeading: ($) =>
      seq(choice(seq(CONTROL, HEADING), CH), choice(FINAL, dataName)),
    reportGroupTypeDetail: ($) => choice(DETAIL, DE),
    reportGroupTypeControlFooting: ($) =>
      seq(choice(seq(CONTROL, FOOTING), CF), choice(FINAL, dataName)),
    reportGroupUsageClause: ($) =>
      seq(optional(seq(USAGE, optional(IS))), choice(DISPLAY, DISPLAY_1)),
    reportGroupTypePageFooting: ($) => choice(seq(PAGE, FOOTING), PF),
    reportGroupTypeReportFooting: ($) => choice(seq(REPORT, FOOTING), RF),
    reportGroupValueClause: ($) => seq(VALUE, optional(IS), literal),
    // -- program library section ----------------------------------
    programLibrarySection: ($) =>
      seq(PROGRAM_LIBRARY, SECTION, DOT_FS, repeat(libraryDescriptionEntry)),
    libraryDescriptionEntry: ($) =>
      choice(libraryDescriptionEntryFormat1, libraryDescriptionEntryFormat2),
    libraryDescriptionEntryFormat1: ($) =>
      seq(
        LD,
        libraryName,
        EXPORT,
        optional(libraryAttributeClauseFormat1),
        optional(libraryEntryProcedureClauseFormat1)
      ),
    libraryDescriptionEntryFormat2: ($) =>
      seq(
        LB,
        libraryName,
        IMPORT,
        optional(libraryIsGlobalClause),
        optional(libraryIsCommonClause),
        repeat(
          choice(
            libraryAttributeClauseFormat2,
            libraryEntryProcedureClauseFormat2
          )
        )
      ),
    libraryAttributeClauseFormat1: ($) =>
      seq(
        ATTRIBUTE,
        optional(
          seq(
            SHARING,
            optional(IS),
            seq(choice(DONTCARE, PRIVATE, SHAREDBYRUNUNIT, SHAREDBYALL))
          )
        )
      ),
    libraryAttributeClauseFormat2: ($) =>
      seq(
        ATTRIBUTE,
        optional(libraryAttributeFunction),
        optional(seq(LIBACCESS, optional(IS), choice(BYFUNCTION, BYTITLE))),
        optional(libraryAttributeParameter),
        optional(libraryAttributeTitle)
      ),
    libraryAttributeFunction: ($) => seq(FUNCTIONNAME, IS, literal),
    libraryAttributeParameter: ($) => seq(LIBPARAMETER, optional(IS), literal),
    libraryAttributeTitle: ($) => seq(TITLE, optional(IS), literal),
    libraryEntryProcedureClauseFormat1: ($) =>
      seq(
        ENTRY_PROCEDURE,
        programName,
        optional(libraryEntryProcedureForClause)
      ),
    libraryEntryProcedureClauseFormat2: ($) =>
      seq(
        ENTRY_PROCEDURE,
        programName,
        optional(libraryEntryProcedureForClause),
        optional(libraryEntryProcedureWithClause),
        optional(libraryEntryProcedureUsingClause),
        optional(libraryEntryProcedureGivingClause)
      ),
    libraryEntryProcedureForClause: ($) => seq(FOR, literal),
    libraryEntryProcedureGivingClause: ($) => seq(GIVING, dataName),
    libraryEntryProcedureUsingClause: ($) =>
      seq(USING, repeat1(libraryEntryProcedureUsingName)),
    libraryEntryProcedureUsingName: ($) => choice(dataName, fileName),
    libraryEntryProcedureWithClause: ($) =>
      seq(WITH, repeat1(libraryEntryProcedureWithName)),
    libraryEntryProcedureWithName: ($) => choice(localName, fileName),
    libraryIsCommonClause: ($) => seq(optional(IS), COMMON),
    libraryIsGlobalClause: ($) => seq(optional(IS), GLOBAL),
    // data description entry ----------------------------------
    dataDescriptionEntry: ($) =>
      choice(
        dataDescriptionEntryFormat1,
        dataDescriptionEntryFormat2,
        dataDescriptionEntryFormat3,
        dataDescriptionEntryExecSql
      ),
    dataDescriptionEntryFormat1: ($) =>
      seq(
        choice(INTEGERLITERAL, LEVEL_NUMBER_77),
        optional(choice(FILLER, dataName)),
        repeat(
          choice(
            dataRedefinesClause,
            dataIntegerStringClause,
            dataExternalClause,
            dataGlobalClause,
            dataTypeDefClause,
            dataThreadLocalClause,
            dataPictureClause,
            dataCommonOwnLocalClause,
            dataTypeClause,
            dataUsingClause,
            dataUsageClause,
            dataValueClause,
            dataReceivedByClause,
            dataOccursClause,
            dataSignClause,
            dataSynchronizedClause,
            dataJustifiedClause,
            dataBlankWhenZeroClause,
            dataWithLowerBoundsClause,
            dataAlignedClause,
            dataRecordAreaClause
          )
        ),
        DOT_FS
      ),
    dataDescriptionEntryFormat2: ($) =>
      seq(LEVEL_NUMBER_66, dataName, dataRenamesClause, DOT_FS),
    dataDescriptionEntryFormat3: ($) =>
      seq(LEVEL_NUMBER_88, conditionName, dataValueClause, DOT_FS),
    dataDescriptionEntryExecSql: ($) =>
      seq(repeat1(EXECSQLLINE), optional(DOT_FS)),
    dataAlignedClause: ($) => ALIGNED,
    dataBlankWhenZeroClause: ($) =>
      seq(BLANK, optional(WHEN), choice(ZERO, ZEROS, ZEROES)),
    dataCommonOwnLocalClause: ($) => seq(choice(COMMON, OWN, LOCAL)),
    dataExternalClause: ($) =>
      seq(optional(IS), EXTERNAL, optionale(seq(BY, literal))),
    dataGlobalClause: ($) => seq(optional(IS), GLOBAL),
    dataIntegerStringClause: ($) => choice(INTEGER, STRING),
    dataJustifiedClause: ($) => seq(choice(JUSTIFIED, JUST), optional(RIGHT)),
    dataOccursClause: ($) =>
      seq(
        OCCURS,
        choice(identifier, integerLiteral),
        optional(dataOccursTo),
        optional(TIMES),
        optional(dataOccursDepending),
        repeat(choice(dataOccursSort, dataOccursIndexed))
      ),
    dataOccursTo: ($) => seq(TO, integerLiteral),
    dataOccursDepending: ($) => seq(DEPENDING, optional(ON), qualifiedDataName),
    dataOccursSort: ($) =>
      seq(
        choice(ASCENDING, DESCENDING),
        optional(KEY),
        optional(IS),
        repeat1(qualifiedDataName)
      ),
    dataOccursIndexed: ($) =>
      seq(INDEXED, optional(BY), optional(LOCAL), repeat1(indexName)),
    dataPictureClause: ($) =>
      seq(choice(PICTURE, PIC), optional(IS), pictureString),
    pictureString: ($) =>
      repeat1(seq(repeat1(pictureChars), optional(pictureCardinality))),
    pictureChars: ($) =>
      choice(
        DOLLARCHAR,
        IDENTIFIER,
        NUMERICLITERAL,
        SLASHCHAR,
        COMMACHAR,
        DOT,
        COLONCHAR,
        ASTERISKCHAR,
        DOUBLEASTERISKCHAR,
        LPARENCHAR,
        RPARENCHAR,
        PLUSCHAR,
        MINUSCHAR,
        LESSTHANCHAR,
        MORETHANCHAR,
        integerLiteral
      ),
    pictureCardinality: ($) => seq(LPARENCHAR, integerLiteral, RPARENCHAR),
    dataReceivedByClause: ($) =>
      seq(optional(RECEIVED), optional(BY), choice(CONTENT, REFERENCE, REF)),
    dataRecordAreaClause: ($) => seq(RECORD, AREA),
    dataRedefinesClause: ($) => seq(REDEFINES, dataName),
    dataRenamesClause: ($) =>
      seq(
        RENAMES,
        qualifiedDataName,
        optional(seq(choice(THROUGH, THRU), qualifiedDataName))
      ),
    dataSignClause: ($) =>
      seq(
        optional(seq(SIGN, optional(IS))),
        choice(LEADING, TRAILING),
        optional(seq(SEPARATE, optional(CHARACTER)))
      ),
    dataSynchronizedClause: ($) =>
      sqe(choice(SYNCHRONIZED, SYNC), optional(choice(LEFT, RIGHT))),
    dataThreadLocalClause: ($) => seq(optional(IS), THREAD_LOCAL),
    dataTypeClause: ($) =>
      seq(
        TYPE,
        optional(IS),
        choice(
          SHORT_DATE,
          LONG_DATE,
          NUMERIC_DATE,
          NUMERIC_TIME,
          LONG_TIME,
          seq(
            choice(CLOB, BLOB, DBCLOB),
            LPARENCHAR,
            integerLiteral,
            RPARENCHAR
          )
        )
      ),
    dataTypeDefClause: ($) => seq(optional(IS), TYPEDEF),
    dataUsageClause: ($) =>
      seq(
        optional(seq(USAGE, optional(IS))),
        choice(
          seq(BINARY, optional(choice(TRUNCATED, EXTENDED))),
          BIT,
          COMP,
          COMP_1,
          COMP_2,
          COMP_3,
          COMP_4,
          COMP_5,
          COMPUTATIONAL,
          COMPUTATIONAL_1,
          COMPUTATIONAL_2,
          COMPUTATIONAL_3,
          COMPUTATIONAL_4,
          COMPUTATIONAL_5,
          CONTROL_POINT,
          DATE,
          DISPLAY,
          DISPLAY_1,
          DOUBLE,
          EVENT,
          FUNCTION_POINTER,
          INDEX,
          KANJI,
          LOCK,
          NATIONAL,
          PACKED_DECIMAL,
          POINTER,
          PROCEDURE_POINTER,
          REAL,
          SQL,
          TASK
        )
      ),
    dataUsingClause: ($) =>
      seq(
        USING,
        choice(LANGUAGE, CONVENTION),
        optional(OF),
        choice(cobolWord, dataName)
      ),
    dataValueClause: ($) =>
      seq(
        optional(seq(choice(VALUE, VALUES), optional(choice(IS, ARE)))),
        dataValueInterval,
        repeat(seq(optional(COMMACHAR), dataValueInterval))
      ),
    dataValueInterval: ($) =>
      seq(dataValueIntervalFrom, optional(dataValueIntervalTo)),
    dataValueIntervalFrom: ($) => choice(literal, cobolWord),
    dataValueIntervalTo: ($) => seq(choice(THROUGH, THRU), literal),
    dataWithLowerBoundsClause: ($) => seq(optional(WITH), LOWER, BOUNDS),
    // --- procedure division --------------------------------------------------------------------
    procedureDivision: ($) =>
      seq(
        PROCEDURE,
        DIVISION,
        optional(procedureDivisionUsingClause),
        optional(procedureDivisionGivingClause),
        DOT_FS,
        optional(procedureDeclaratives),
        procedureDivisionBody
      ),
    procedureDivisionUsingClause: ($) =>
      seq(choice(USING, CHAINING), repeat1(procedureDivisionUsingParameter)),
    procedureDivisionGivingClause: ($) =>
      seq(choice(GIVING, RETURNING), dataName),
    procedureDivisionUsingParameter: ($) =>
      choice(
        procedureDivisionByReferencePhrase,
        procedureDivisionByValuePhrase
      ),
    procedureDivisionByReferencePhrase: ($) =>
      seq(
        optional(seq(optional(BY), REFERENCE)),
        repeat1(procedureDivisionByReference)
      ),
    procedureDivisionByReference: ($) =>
      //(OPTIONAL? (identifier | fileName)) | ANY
      choice(seq(optional(OPTIONAL), choice(identifier, fileName)), ANY),
    procedureDivisionByValuePhrase: ($) =>
      seq(optional(BY), VALUE, repeat1(procedureDivisionByValue)),
    procedureDivisionByValue: ($) => choice(identifier, literal, ANY),
    procedureDeclaratives: ($) =>
      seq(
        DECLARATIVES,
        DOT_FS,
        repeat1(procedureDeclarative),
        END,
        DECLARATIVES,
        DOT_FS
      ),
    procedureDeclarative: ($) =>
      seq(procedureSectionHeader, DOT_FS, useStatement, DOT_FS, paragraphs),
    procedureSectionHeader: ($) =>
      seq(sectionName, SECTION, optional(integerLiteral)),
    procedureDivisionBody: ($) => seq(paragraphs, repeat(procedureSection)),
    // -- procedure section ----------------------------------
    procedureSection: ($) => seq(procedureSectionHeader, DOT_FS, paragraphs),
    paragraphs: ($) => seq(repeat(sentence), repeat(paragraph)),
    paragraph: ($) =>
      seq(
        paragraphName,
        optional(DOT_FS),
        choice(alteredGoTo, repeat(sentence))
      ),
    sentence: ($) => seq(repeat(statement), DOT_FS),
    statement: ($) =>
      choice(
        acceptStatement,
        addStatement,
        alterStatement,
        callStatement,
        cancelStatement,
        closeStatement,
        computeStatement,
        continueStatement,
        deleteStatement,
        disableStatement,
        displayStatement,
        divideStatement,
        enableStatement,
        entryStatement,
        evaluateStatement,
        exhibitStatement,
        execCicsStatement,
        execSqlStatement,
        execSqlImsStatement,
        exitStatement,
        generateStatement,
        gobackStatement,
        goToStatement,
        ifStatement,
        initializeStatement,
        initiateStatement,
        inspectStatement,
        mergeStatement,
        moveStatement,
        multiplyStatement,
        nextSentenceStatement,
        openStatement,
        performStatement,
        purgeStatement,
        readStatement,
        receiveStatement,
        releaseStatement,
        returnStatement,
        rewriteStatement,
        searchStatement,
        sendStatement,
        setStatement,
        sortStatement,
        startStatement,
        stopStatement,
        stringStatement,
        subtractStatement,
        terminateStatement,
        unstringStatement,
        writeStatement
      ),
    // accept statement
    acceptStatement: ($) =>
      seq(
        ACCEPT,
        identifier,
        optionL(
          choice(
            acceptFromDateStatement,
            acceptFromEscapeKeyStatement,
            acceptFromMnemonicStatement,
            acceptMessageCountStatement
          )
        ),
        optional(onExceptionClause),
        optional(notOnExceptionClause),
        optional(END_ACCEPT)
      ),
    acceptFromDateStatement: ($) =>
      seq(
        FROM,
        seq(
          DATE,
          choice(optional(YYYYMMDD), DAY),
          choice(optional(YYYYDDD), DAY_OF_WEEK, TIME, TIMER, TODAYS_DATE),
          choice(optional(MMDDYYYY), TODAYS_NAME, YEAR, YYYYMMDD, YYYYDDD)
        )
      ),
    acceptFromMnemonicStatement: ($) => seq(FROM, mnemonicName),
    acceptFromEscapeKeyStatement: ($) => seq(FROM, ESCAPE, KEY),
    acceptMessageCountStatement: ($) => seq(optional(MESSAGE), COUNT),
    // add statement
    addStatement: ($) =>
      seq(
        ADD,
        choice(addToStatement, addToGivingStatement, addCorrespondingStatement),
        optional(onSizeErrorPhrase),
        optional(notOnSizeErrorPhrase),
        optional(END_ADD)
      ),
    addToStatement: ($) => seq(repeat1(addFrom), TO, repeat1(addTo)),
    addToGivingStatement: ($) =>
      seq(
        repeat1(addFrom),
        optional(seq(TO, repeat1(addToGiving))),
        GIVING,
        repeat1(addGiving)
      ),
    addCorrespondingStatement: ($) =>
      seq(choice(CORRESPONDING, CORR), identifier, TO, addTo),
    addFrom: ($) => choice(identifier, literal),
    addTo: ($) => seq(identifier, optional(ROUNDED)),
    addToGiving: ($) => choice(identifier, literal),
    addGiving: ($) => seq(identifier, optional(ROUNDED)),
    // altered go to statement
    alteredGoTo: ($) => seq(GO, optional(TO), DOT_FS),
    // alter statement
    alterStatement: ($) => seq(ALTER, repeat1(alterProceedTo)),
    alterProceedTo: ($) =>
      seq(procedureName, TO, optional(seq(PROCEED, TO)), procedureName),
    // call statement
    callStatement: ($) =>
      seq(
        CALL,
        choice(identifier, literal),
        optional(callUsingPhrase),
        optional(callGivingPhrase),
        optional(onOverflowPhrase),
        optional(onExceptionClause),
        optional(notOnExceptionClause),
        optional(END_CALL)
      ),
    callUsingPhrase: ($) => seq(USING, repeat1(callUsingParameter)),
    callUsingParameter: ($) =>
      choice(callByReferencePhrase, callByValuePhrase, callByContentPhrase),
    callByReferencePhrase: ($) =>
      seq(optional(seq(optional(BY), REFERENCE)), repeat1(callByReference)),
    callByReference: ($) =>
      choice(
        choice(
          seq(optional(choice(seq(ADDRESS, OF), INTEGER, STRING)), identifier),
          literal,
          fileName
        ),
        OMITTED
      ),
    callByValuePhrase: ($) => seq(optional(BY), VALUE, repeat1(callByValue)),
    callByValue: ($) =>
      seq(
        optional(seq(ADDRESS, choice(OF, LENGTH), optional(OF))),
        choice(identifier, literal)
      ),
    callByContentPhrase: ($) =>
      seq(optional(BY), CONTENT, repeat1(callByContent)),
    callByContent: ($) =>
      seq(
        optional(choice(seq(ADDRESS, OF), seq(LENGTH, optional(OF)))),
        choice(identifier, literal, OMITTED)
      ),
    callGivingPhrase: ($) => seq(choice(GIVING, RETURNING), identifier),
    // cancel statement
    cancelStatement: ($) => seq(CANCEL, repeat1(cancelCall)),
    cancelCall: ($) =>
      seq(libraryName, choice(BYTITLE, BYFUNCTION, identifier, literal)),
    // close statement
    closeStatement: ($) => seq(LOSE, repeat1(closeFile)),
    closeFile: ($) =>
      seq(
        fileName,
        optional(
          choice(
            closeReelUnitStatement,
            closeRelativeStatement,
            closePortFileIOStatement
          )
        )
      ),
    closeReelUnitStatement: ($) =>
      seq(
        choice(REEL, UNIT),
        optional(seq(optional(FOR), REMOVAL)),
        optional(seq(optional(WITH), choice(seq(NO, REWIND), LOCK)))
      ),
    closeRelativeStatement: ($) =>
      seq(optional(WITH), choice(seq(NO, REWIND), LOCK)),
    closePortFileIOStatement: ($) =>
      seq(
        choice(seq(optional(WITH), NO, WAIT), seq(WITH, WAIT)),
        optional(USING, repeat1(closePortFileIOUsing))
      ),
    closePortFileIOUsing: ($) =>
      choice(
        closePortFileIOUsingCloseDisposition,
        closePortFileIOUsingAssociatedData,
        closePortFileIOUsingAssociatedDataLength
      ),
    closePortFileIOUsingCloseDisposition: ($) =>
      seq(CLOSE_DISPOSITION, optional(OF), choice(ABORT, ORDERLY)),
    closePortFileIOUsingAssociatedData: ($) =>
      seq(ASSOCIATED_DATA, choice(identifier, integerLiteral)),
    closePortFileIOUsingAssociatedDataLength: ($) =>
      seq(
        ASSOCIATED_DATA_LENGTH,
        optional(OF),
        choice(identifier, integerLiteral)
      ),
    // compute statement
    computeStatement: ($) =>
      seq(
        COMPUTE,
        repeat1(computeStore),
        choice(EQUALCHAR, EQUAL),
        arithmeticExpression,
        optional(onSizeErrorPhrase),
        optional(notOnSizeErrorPhrase),
        optional(END_COMPUTE)
      ),
    computeStore: ($) => seq(identifier, optional(ROUNDED)),
    // continue statement
    continueStatement: ($) => CONTINUE,
    // delete statement
    deleteStatement: ($) =>
      seq(
        DELETE,
        fileName,
        optional(RECORD),
        optional(invalidKeyPhrase),
        optional(notInvalidKeyPhrase),
        optional(END_DELETE)
      ),
    // disable statement
    disableStatement: ($) =>
      seq(
        DISABLE,
        choice(seq(INPUT, optional(TERMINAL)), seq(I_O, TERMINAL), OUTPUT),
        cdName,
        optional(WITH),
        KEY,
        choice(identifier, literal)
      ),
    // display statement
    displayStatement: ($) =>
      seq(
        DISPLAY,
        repeat1(displayOperand),
        optional(displayAt),
        optional(displayUpon),
        optional(displayWith),
        optional(onExceptionClause),
        optional(notOnExceptionClause),
        optional(END_DISPLAY)
      ),
    displayOperand: ($) => choice(identifier, literal),
    displayAt: ($) => seq(AT, choice(identifier, literal)),
    displayUpon: ($) => seq(UPON, choice(mnemonicName, environmentName)),
    displayWith: ($) => seq(optional(WITH), NO, ADVANCING),
    // divide statement
    divideStatement: ($) =>
      seq(
        DIVIDE,
        choice(identifier, literal),
        choice(
          divideIntoStatement,
          divideIntoGivingStatement,
          divideByGivingStatement
        ),
        optional(divideRemainder),
        optional(onSizeErrorPhrase),
        optional(notOnSizeErrorPhrase),
        optional(END_DIVIDE)
      ),
    divideIntoStatement: ($) => seq(INTO, repeat1(divideInto)),
    divideIntoGivingStatement: ($) =>
      seq(INTO, choice(identifier, literal), optional(divideGivingPhrase)),
    divideByGivingStatement: ($) =>
      seq(BY, choice(identifier, literal), optional(divideGivingPhrase)),
    divideGivingPhrase: ($) => seq(GIVING, repeat1(divideGiving)),
    divideInto: ($) => seq(identifier, optional(ROUNDED)),
    divideGiving: ($) => seq(identifier, optional(ROUNDED)),
    divideRemainder: ($) => seq(REMAINDER, identifier),
    // enable statement
    enableStatement: ($) =>
      seq(
        ENABLE,
        choice(seq(INPUT, optional(TERMINAL)), seq(I_O, TERMINAL), OUTPUT),
        cdName,
        optional(WITH),
        KEY,
        choice(literal, identifier)
      ),
    // entry statement
    entryStatement: ($) =>
      seq(ENTRY, literal, optional(seq(USING, repeat1(identifier)))),
    // evaluate statement
    evaluateStatement: ($) =>
      seq(
        EVALUATE,
        evaluateSelect,
        repeat(evaluateAlsoSelect),
        repeat(evaluateWhenPhrase),
        optional(evaluateWhenOther),
        optional(END_EVALUATE)
      ),
    evaluateSelect: ($) =>
      choice(identifier, literal, arithmeticExpression, condition),
    evaluateAlsoSelect: ($) => seq(ALSO, evaluateSelect),
    evaluateWhenPhrase: ($) => seq(repeat1(evaluateWhen), repeat(statement)),
    evaluateWhen: ($) =>
      seq(WHEN, evaluateCondition, repeat(evaluateAlsoCondition)),
    evaluateCondition: ($) =>
      seq(
        choice(ANY, optional(NOT)),
        evaluateValue,
        choice(optional(evaluateThrough), condition, booleanLiteral)
      ),
    evaluateThrough: ($) => seq(choice(THROUGH, THRU), evaluateValue),
    evaluateAlsoCondition: ($) => seq(ALSO, evaluateCondition),
    evaluateWhenOther: ($) => seq(WHEN, OTHER, repeat(statement)),
    evaluateValue: ($) => choice(identifier, literal, arithmeticExpression),
    // exec cics statement
    execCicsStatement: ($) => repeat1(EXECCICSLINE),
    // exec sql statement
    execSqlStatement: ($) => repeat1(EXECSQLLINE),
    // exec sql ims statement
    execSqlImsStatement: ($) => repeat1(EXECSQLIMSLINE),
    // exhibit statement
    exhibitStatement: ($) =>
      seq(EXHIBIT, optional(NAMED), optional(CHANGED), repeat1(exhibitOperand)),
    exhibitOperand: ($) => choice(identifier, literal),
    // exit statement
    exitStatement: ($) => seq(EXIT, optional(PROGRAM)),
    // generate statement
    generateStatement: ($) => seq(GENERATE, reportName),
    // goback statement
    gobackStatement: ($) => GOBACK,
    // goto statement
    goToStatement: ($) =>
      seq(
        GO,
        optional(TO),
        choice(goToStatementSimple, goToDependingOnStatement)
      ),
    goToStatementSimple: ($) => procedureName,
    goToDependingOnStatement: ($) =>
      seq(
        choice(MORE_LABELS, repeat1(procedureName)),
        optional(seq(DEPENDING, optional(ON), identifier))
      ),
    // if statement
    ifStatement: ($) =>
      seq(IF, condition, ifThen, optional(ifElse), optional(END_IF)),
    ifThen: ($) =>
      seq(optional(THEN), choice(seq(NEXT, SENTENCE), repeat(statement))),
    ifElse: ($) => seq(ELSE, choice(seq(NEXT, SENTENCE), repeat(statement))),
    // initialize statement
    initializeStatement: ($) =>
      seq(INITIALIZE, repeat1(identifier), optional(initializeReplacingPhrase)),
    initializeReplacingPhrase: ($) =>
      seq(REPLACING, repeat1(initializeReplacingBy)),
    initializeReplacingBy: ($) =>
      seq(
        choice(
          ALPHABETIC,
          ALPHANUMERIC,
          ALPHANUMERIC_EDITED,
          NATIONAL,
          NATIONAL_EDITED,
          NUMERIC,
          NUMERIC_EDITED,
          DBCS,
          EGCS
        ),
        optional(DATA),
        BY,
        choice(identifier, literal)
      ),
    // initiate statement
    initiateStatement: ($) => seq(INITIATE, repeat1(reportName)),
    // inspect statement
    inspectStatement: ($) =>
      seq(
        INSPECT,
        identifier,
        choice(
          inspectTallyingPhrase,
          inspectReplacingPhrase,
          inspectTallyingReplacingPhrase,
          inspectConvertingPhrase
        )
      ),
    inspectTallyingPhrase: ($) => seq(TALLYING, repeat1(inspectFor)),
    inspectReplacingPhrase: ($) =>
      seq(
        REPLACING,
        repeat1(choice(inspectReplacingCharacters, inspectReplacingAllLeadings))
      ),
    inspectTallyingReplacingPhrase: ($) =>
      seq(TALLYING, repeat1(inspectFor), repeat1(inspectReplacingPhrase)),
    inspectConvertingPhrase: ($) =>
      seq(
        CONVERTING,
        choice(identifier, literal),
        inspectTo,
        repeat(inspectBeforeAfter)
      ),
    inspectFor: ($) =>
      seq(
        identifier,
        FOR,
        repeat1(choice(inspectCharacters, inspectAllLeadings))
      ),
    inspectCharacters: ($) =>
      seq(choice(CHARACTER, CHARACTERS), repeat(inspectBeforeAfter)),
    inspectReplacingCharacters: ($) =>
      seq(choice(CHARACTER, CHARACTERS), inspectBy, repeat(inspectBeforeAfter)),
    inspectAllLeadings: ($) =>
      seq(choice(ALL, LEADING), repeat1(inspectAllLeading)),
    inspectReplacingAllLeadings: ($) =>
      seq(choice(ALL, LEADING, FIRST), repeat1(inspectReplacingAllLeading)),
    inspectAllLeading: ($) =>
      seq(choice(identifier, literal), repeat(inspectBeforeAfter)),
    inspectReplacingAllLeading: ($) =>
      seq(choice(identifier, literal), inspectBy, repeat(inspectBeforeAfter)),
    inspectBy: ($) => seq(BY, choice(identifier, literal)),
    inspectTo: ($) => seq(TO, choice(identifier, literal)),
    inspectBeforeAfter: ($) =>
      seq(
        choice(BEFORE, AFTER),
        optional(INITIAL),
        choice(identifier, literal)
      ),
    // merge statement
    mergeStatement: ($) =>
      seq(
        MERGE,
        fileName,
        repeat1(mergeOnKeyClause),
        optional(mergeCollatingSequencePhrase),
        repeat(mergeUsing),
        optional(mergeOutputProcedurePhrase),
        repeat(mergeGivingPhrase)
      ),
    mergeOnKeyClause: ($) =>
      seq(
        optional(ON),
        choice(ASCENDING, DESCENDING),
        optional(KEY),
        repeat1(qualifiedDataName)
      ),
    mergeCollatingSequencePhrase: ($) =>
      seq(
        optional(COLLATING),
        SEQUENCE,
        optional(IS),
        repeat1(alphabetName),
        optional(mergeCollatingAlphanumeric),
        optional(mergeCollatingNational)
      ),
    mergeCollatingAlphanumeric: ($) =>
      seq(optional(FOR), ALPHANUMERIC, IS, alphabetName),
    mergeCollatingNational: ($) =>
      seq(optional(FOR), NATIONAL, optional(IS), alphabetName),
    mergeUsing: ($) => seq(USING, repeat1(fileName)),
    mergeOutputProcedurePhrase: ($) =>
      seq(
        OUTPUT,
        PROCEDURE,
        optional(IS),
        procedureName,
        optional(mergeOutputThrough)
      ),
    mergeOutputThrough: ($) => seq(choice(THROUGH, THRU), procedureName),
    mergeGivingPhrase: ($) => seq(GIVING, repeat1(mergeGiving)),
    mergeGiving: ($) =>
      seq(
        fileName,
        optional(
          seq(
            choice(LOCK, SAVE, NO),
            choice(REWIND, CRUNCH, RELEASE, WITH),
            REMOVE,
            CRUNCH
          )
        )
      ),
    // move statement
    moveStatement: ($) =>
      seq(
        MOVE,
        optional(ALL),
        choice(moveToStatement, moveCorrespondingToStatement)
      ),
    moveToStatement: ($) => seq(moveToSendingArea, TO, repeat1(identifier)),
    moveToSendingArea: ($) => choice(identifier, literal),
    moveCorrespondingToStatement: ($) =>
      seq(
        choice(CORRESPONDING, CORR),
        moveCorrespondingToSendingArea,
        TO,
        repeat1(identifier)
      ),
    moveCorrespondingToSendingArea: ($) => identifier,
    // multiply statement
    multiplyStatement: ($) =>
      seq(
        MULTIPLY,
        choice(identifier, literal),
        BY,
        choice(multiplyRegular, multiplyGiving),
        optional(onSizeErrorPhrase),
        optional(notOnSizeErrorPhrase),
        optional(END_MULTIPLY)
      ),
    multiplyRegular: ($) => repeat1(multiplyRegularOperand),
    multiplyRegularOperand: ($) => seq(identifier, optional(ROUNDED)),
    multiplyGiving: ($) =>
      seq(multiplyGivingOperand, GIVING, repeat1(multiplyGivingResult)),
    multiplyGivingOperand: ($) => choice(identifier, literal),
    multiplyGivingResult: ($) => seq(identifier, optional(ROUNDED)),
    // next sentence
    nextSentenceStatement: ($) => seq(NEXT, SENTENCE),
    // open statement
    openStatement: ($) =>
      seq(
        OPEN,
        repeat1(
          choice(
            openInputStatement,
            openOutputStatement,
            openIOStatement,
            openExtendStatement
          )
        )
      ),
    openInputStatement: ($) => seq(INPUT, repeat1(openInput)),
    openInput: ($) =>
      seq(fileName, choice(REVERSED, seq(optional(WITH), NO, REWIND))),
    openOutputStatement: ($) => seq(OUTPUT, repeat1(openOutput)),
    openOutput: ($) => seq(fileName, optional(seq(optional(WITH), NO, REWIND))),
    openIOStatement: ($) => seq(I_O, repeat1(fileName)),
    openExtendStatement: ($) => seq(EXTEND, repeat1(fileName)),
    // perform statement
    performStatement: ($) =>
      seq(PERFORM, choice(performInlineStatement, performProcedureStatement)),
    performInlineStatement: ($) =>
      seq(optional(performType), repeat(statement), END_PERFORM),
    performProcedureStatement: ($) =>
      seq(
        procedureName,
        optional(seq(choice(THROUGH, THRU), procedureName)),
        optional(performType)
      ),
    performType: ($) => choice(performTimes, performUntil, performVarying),
    performTimes: ($) => seq(choice(identifier, integerLiteral), TIMES),
    performUntil: ($) => seq(optional(performTestClause), UNTIL, condition),
    performVarying: ($) =>
      seq(
        performTestClause,
        choice(performVaryingClause, performVaryingClause),
        optional(performTestClause)
      ),
    performVaryingClause: ($) =>
      seq(VARYING, performVaryingPhrase, repeat(performAfter)),
    performVaryingPhrase: ($) =>
      seq(choice(identifier, literal), performFrom, performBy, performUntil),
    performAfter: ($) => seq(AFTER, performVaryingPhrase),
    performFrom: ($) =>
      seq(FROM, choice(identifier, literal, arithmeticExpression)),
    performBy: ($) =>
      seq(BY, choice(identifier, literal, arithmeticExpression)),
    performTestClause: ($) => seq(optional(WITH), TEST, choice(BEFORE, AFTER)),
    // purge statement
    purgeStatement: ($) => seq(PURGE, repeat1(cdName)),
    // read statement
    readStatement: ($) =>
      seq(
        READ,
        fileName,
        optional(NEXT),
        optional(RECORD),
        optional(readInto),
        optional(readWith),
        optional(readKey),
        optional(invalidKeyPhrase),
        optional(notInvalidKeyPhrase),
        optional(atEndPhrase),
        optional(notAtEndPhrase),
        optional(END_READ)
      ),
    readInto: ($) => seq(INTO, identifier),
    readWith: ($) =>
      seq(optional(WITH), choice(seq(choice(KEPT, NO), LOCK), WAIT)),
    readKey: ($) => seq(KEY, optional(IS), qualifiedDataName),
    // receive statement
    receiveStatement: ($) =>
      seq(
        RECEIVE,
        choice(receiveFromStatement, receiveIntoStatement),
        optional(onExceptionClause),
        optional(notOnExceptionClause),
        optional(END_RECEIVE)
      ),
    receiveFromStatement: ($) =>
      seq(
        dataName,
        FROM,
        receiveFrom,
        repeat(
          choice(
            receiveBefore,
            receiveWith,
            receiveThread,
            receiveSize,
            receiveStatus
          )
        )
      ),
    receiveFrom: ($) =>
      choice(seq(THREAD, dataName), seq(LAST, THREAD), seq(ANY, THREAD)),
    receiveIntoStatement: ($) =>
      seq(
        cdName,
        choice(MESSAGE, SEGMENT),
        optional(INTO),
        identifier,
        optional(receiveNoData),
        optional(receiveWithData)
      ),
    receiveNoData: ($) => seq(NO, DATA, repeat(statement)),
    receiveWithData: ($) => seq(WITH, DATA, repeat(statement)),
    receiveBefore: ($) =>
      seq(BEFORE, optional(TIME), choice(numericLiteral, identifier)),
    receiveWith: ($) => seq(optional(WITH), NO, WAIT),
    receiveThread: ($) => seq(THREAD, optional(IN), dataName),
    receiveSize: ($) =>
      seq(SIZE, optional(IN), choice(numericLiteral, identifier)),
    receiveStatus: ($) => seq(STATUS, optional(IN), identifier),
    // release statement
    releaseStatement: ($) =>
      seq(RELEASE, recordName, optionql(seq(FROM, qualifiedDataName))),
    // return statement
    returnStatement: ($) =>
      seq(
        RETURN,
        fileName,
        optional(RECORD),
        optional(returnInto),
        atEndPhrase,
        optional(notAtEndPhrase),
        optional(END_RETURN)
      ),
    returnInto: ($) => seq(INTO, qualifiedDataName),
    // rewrite statement
    rewriteStatement: ($) =>
      seq(
        REWRITE,
        recordName,
        optional(rewriteFrom),
        optional(invalidKeyPhrase),
        optional(notInvalidKeyPhrase),
        optional(END_REWRITE)
      ),
    rewriteFrom: ($) => seq(FROM, identifier),
    // search statement
    searchStatement: ($) =>
      seq(
        SEARCH,
        optional(ALL),
        qualifiedDataName,
        optional(searchVarying),
        optional(atEndPhrase),
        repeat1(searchWhen),
        optional(END_SEARCH)
      ),
    searchVarying: ($) => seq(VARYING, qualifiedDataName),
    searchWhen: ($) =>
      seq(WHEN, condition, choice(seq(NEXT, SENTENCE), repeat(statement))),
    // send statement
    sendStatement: ($) =>
      seq(
        SEND,
        choice(sendStatementSync, sendStatementAsync),
        optional(onExceptionClause),
        optional(notOnExceptionClause)
      ),
    sendStatementSync: ($) =>
      seq(
        choice(identifier, literal),
        optional(sendFromPhrase),
        optional(sendWithPhrase),
        optional(sendReplacingPhrase),
        optional(sendAdvancingPhrase)
      ),
    sendStatementAsync: ($) => seq(TO, choice(TOP, BOTTOM), identifier),
    sendFromPhrase: ($) => seq(FROM, identifier),
    sendWithPhrase: ($) => seq(WITH, choice(EGI, EMI, ESI, identifier)),
    sendReplacingPhrase: ($) => seq(REPLACING, optional(LINE)),
    sendAdvancingPhrase: ($) =>
      seq(
        choice(BEFORE, AFTER),
        optional(ADVANCING),
        choice(sendAdvancingPage, sendAdvancingLines, sendAdvancingMnemonic)
      ),
    sendAdvancingPage: ($) => PAGE,
    sendAdvancingLines: ($) =>
      seq(choice(identifier, literal), optional(choice(LINE, LINES))),
    sendAdvancingMnemonic: ($) => mnemonicName,
    // set statement
    setStatement: ($) =>
      seq(SET, choice(repeat1(setToStatement), setUpDownByStatement)),
    setToStatement: ($) => seq(repeat1(setTo), TO, repeat1(setToValue)),
    setUpDownByStatement: ($) =>
      seq(repeat1(setTo), choice(seq(UP, BY), seq(DOWN, BY)), setByValue),
    setTo: ($) => identifier,
    setToValue: ($) =>
      seq(
        choice(
          ON,
          OFF,
          seq(ENTRY, choice(identifier, literal)),
          identifier,
          literal
        )
      ),
    setByValue: ($) => choice(identifier, literal),
    // sort statement
    sortStatement: ($) =>
      seq(
        SORT,
        fileName,
        repeat1(sortOnKeyClause),
        optional(sortDuplicatesPhrase),
        optional(sortCollatingSequencePhrase),
        optional(sortInputProcedurePhrase),
        repeat(sortUsing),
        optional(sortOutputProcedurePhrase),
        repeat(sortGivingPhrase)
      ),
    sortOnKeyClause: ($) =>
      seq(
        optional(ON),
        choice(ASCENDING, DESCENDING),
        optional(KEY),
        repeat1(qualifiedDataName)
      ),
    sortDuplicatesPhrase: ($) =>
      seq(optional(WITH), DUPLICATES, optional(IN), optional(ORDER)),
    sortCollatingSequencePhrase: ($) =>
      seq(
        optional(COLLATING),
        SEQUENCE,
        optional(IS),
        repeat1(alphabetName),
        optional(sortCollatingAlphanumeric),
        optional(sortCollatingNational)
      ),
    sortCollatingAlphanumeric: ($) =>
      seq(optional(FOR), ALPHANUMERIC, IS, alphabetName),
    sortCollatingNational: ($) =>
      seq(optional(FOR), NATIONAL, optional(IS), alphabetName),
    sortInputProcedurePhrase: ($) =>
      seq(
        INPUT,
        PROCEDURE,
        optional(IS),
        procedureName,
        optional(sortInputThrough)
      ),
    sortInputThrough: ($) => seq(choice(THROUGH, THRU), procedureName),
    sortUsing: ($) => seq(USING, repeat1(fileName)),
    sortOutputProcedurePhrase: ($) =>
      seq(
        OUTPUT,
        PROCEDURE,
        optional(IS),
        procedureName,
        optional(sortOutputThrough)
      ),
    sortOutputThrough: ($) => seq(choice(THROUGH, THRU), procedureName),
    sortGivingPhrase: ($) => seq(GIVING, repeat1(sortGiving)),
    sortGiving: ($) =>
      seq(
        fileName,
        option(
          choice(
            LOCK,
            SAVE,
            seq(NO, REWIND),
            CRUNCH,
            RELEASE,
            seq(WITH, REMOVE, CRUNCH)
          )
        )
      ),
    // start statement
    startStatement: ($) =>
      seq(
        START,
        fileName,
        optional(startKey),
        optional(invalidKeyPhrase),
        optional(notInvalidKeyPhrase),
        optional(END_START)
      ),
    startKey: ($) =>
      seq(
        KEY,
        optional(IS),
        choice(
          seq(EQUAL, optional(TO)),
          EQUALCHAR,
          seq(GREATER, optional(THAN)),
          MORETHANCHAR,
          seq(NOT, LESS, optional(THAN)),
          seq(NOT, LESSTHANCHAR),
          seq(GREATER, optional(THAN), OR, EQUAL, optional(TO)),
          MORETHANOREQUAL
        ),
        qualifiedDataName
      ),
    // stop statement
    stopStatement: ($) => seq(STOP, choice(RUN, literal, stopStatementGiving)),
    stopStatementGiving: ($) =>
      seq(RUN, choice(GIVING, RETURNING), choice(identifier, integerLiteral)),
    // string statement
    stringStatement: ($) =>
      seq(
        STRING,
        repeat1(stringSendingPhrase),
        stringIntoPhrase,
        optional(stringWithPointerPhrase),
        optional(onOverflowPhrase),
        optional(notOnOverflowPhrase),
        optional(END_STRING)
      ),
    stringSendingPhrase: ($) =>
      seq(
        stringSending,
        repeat(seq(optional(COMMACHAR), stringSending)),
        choice(stringDelimitedByPhrase, stringForPhrase)
      ),
    stringSending: ($) => choice(identifier, literal),
    stringDelimitedByPhrase: ($) =>
      seq(DELIMITED, optional(BY), choice(SIZE, identifier, literal)),
    stringForPhrase: ($) => seq(FOR, choice(identifier, literal)),
    stringIntoPhrase: ($) => seq(INTO, identifier),
    stringWithPointerPhrase: ($) =>
      seq(optional(WITH), POINTER, qualifiedDataName),
    // subtract statement
    subtractStatement: ($) =>
      seq(
        SUBTRACT,
        choice(
          subtractFromStatement,
          subtractFromGivingStatement,
          subtractCorrespondingStatement
        ),
        optional(onSizeErrorPhrase),
        optional(notOnSizeErrorPhrase),
        optional(END_SUBTRACT)
      ),
    subtractFromStatement: ($) =>
      seq(repeat1(subtractSubtrahend), FROM, repeat1(subtractMinuend)),
    subtractFromGivingStatement: ($) =>
      seq(
        repeat1(subtractSubtrahend),
        FROM,
        subtractMinuendGiving,
        GIVING,
        repeat1(subtractGiving)
      ),
    subtractCorrespondingStatement: ($) =>
      seq(
        choice(CORRESPONDING, CORR),
        qualifiedDataName,
        FROM,
        subtractMinuendCorresponding
      ),
    subtractSubtrahend: ($) => choice(identifier, literal),
    subtractMinuend: ($) => seq(identifier, optional(ROUNDED)),
    subtractMinuendGiving: ($) => choice(identifier, literal),
    subtractGiving: ($) => seq(identifier, optional(ROUNDED)),
    subtractMinuendCorresponding: ($) =>
      seq(qualifiedDataName, optional(ROUNDED)),
    // terminate statement
    terminateStatement: ($) => seq(TERMINATE, reportName),
    // unstring statement
    unstringStatement: ($) =>
      seq(
        UNSTRING,
        unstringSendingPhrase,
        unstringIntoPhrase,
        optional(unstringWithPointerPhrase),
        optional(unstringTallyingPhrase),
        optional(onOverflowPhrase),
        optional(notOnOverflowPhrase),
        optional(END_UNSTRING)
      ),
    unstringSendingPhrase: ($) =>
      seq(
        identifier,
        optional(seq(unstringDelimitedByPhrase, repeat(unstringOrAllPhrase)))
      ),
    unstringDelimitedByPhrase: ($) =>
      seq(DELIMITED, optional(BY), optional(ALL), choice(identifier, literal)),
    unstringOrAllPhrase: ($) =>
      seq(OR, optional(ALL), choice(identifier, literal)),
    unstringIntoPhrase: ($) => seq(INTO, repeat1(unstringInto)),
    unstringInto: ($) =>
      seq(identifier, optional(unstringDelimiterIn), optional(unstringCountIn)),
    unstringDelimiterIn: ($) => seq(DELIMITER, optional(IN), identifier),
    unstringCountIn: ($) => seq(COUNT, optional(IN), identifier),
    unstringWithPointerPhrase: ($) =>
      seq(optional(WITH), POINTER, qualifiedDataName),
    unstringTallyingPhrase: ($) =>
      seq(TALLYING, optional(IN), qualifiedDataName),
    // use statement
    useStatement: ($) => seq(USE, choice(useAfterClause, useDebugClause)),
    useAfterClause: ($) =>
      seq(
        optional(GLOBAL),
        AFTER,
        optional(STANDARD),
        choice(EXCEPTION, ERROR),
        PROCEDURE,
        optional(ON),
        useAfterOn
      ),
    useAfterOn: ($) =>
      seq(choice(INPUT, OUTPUT, I_O, EXTEND, repeat1(fileName))),
    useDebugClause: ($) =>
      seq(optional(FOR), DEBUGGING, optional(ON), repeat1(useDebugOn)),
    useDebugOn: ($) =>
      seq(
        ALL,
        choice(PROCEDURES, ALL),
        optional(REFERENCES),
        optional(OF),
        choice(identifier, procedureName, fileName)
      ),
    // write statement
    writeStatement: ($) =>
      seq(
        WRITE,
        recordName,
        optional(writeFromPhrase),
        optional(writeAdvancingPhrase),
        optional(writeAtEndOfPagePhrase),
        optional(writeNotAtEndOfPagePhrase),
        optional(invalidKeyPhrase),
        optional(notInvalidKeyPhrase),
        optional(END_WRITE)
      ),
    writeFromPhrase: ($) => seq(FROM, choice(identifier, literal)),
    writeAdvancingPhrase: ($) =>
      seq(
        choice(BEFORE, AFTER),
        optional(ADVANCING),
        choice(writeAdvancingPage, writeAdvancingLines, writeAdvancingMnemonic)
      ),
    writeAdvancingPage: ($) => PAGE,
    writeAdvancingLines: ($) =>
      seq(choice(identifier, literal), optional(choice(LINE, LINES))),
    writeAdvancingMnemonic: ($) => mnemonicName,
    writeAtEndOfPagePhrase: ($) =>
      seq(optional(AT), choice(END_OF_PAGE, EOP), repeat(statement)),
    writeNotAtEndOfPagePhrase: ($) =>
      seq(NOT, optional(AT), choice(END_OF_PAGE, EOP), repeat(statement)),
    // statement phrases ----------------------------------
    atEndPhrase: ($) => seq(optional(AT), END, repeat(statement)),
    notAtEndPhrase: ($) => seq(NOT, optional(AT), END, repeat(statement)),
    invalidKeyPhrase: ($) => seq(INVALID, optional(KEY), repeat(statement)),
    notInvalidKeyPhrase: ($) =>
      seq(NOT, INVALID, optional(KEY), repeat(statement)),
    onOverflowPhrase: ($) => seq(optional(ON), OVERFLOW, repeat(statement)),
    notOnOverflowPhrase: ($) =>
      seq(NOT, optional(ON), OVERFLOW, repeat(statement)),
    onSizeErrorPhrase: ($) => seq(optional(ON), SIZE, ERROR, repeat(statement)),
    notOnSizeErrorPhrase: ($) =>
      seq(NOT, optional(ON), SIZE, ERROR, repeat(statement)),
    // statement clauses ----------------------------------
    onExceptionClause: ($) => seq(optional(ON), EXCEPTION, repeat(statement)),
    notOnExceptionClause: ($) =>
      seq(NOT, optional(ON), EXCEPTION, repeat(statement)),
    // arithmetic expression ----------------------------------
    arithmeticExpression: ($) => seq(multDivs, repeat(plusMinus)),
    plusMinus: ($) => seq(choice(PLUSCHAR, MINUSCHAR), multDivs),
    multDivs: ($) => seq(powers, repeat(multDiv)),
    multDiv: ($) => seq(choice(ASTERISKCHAR, SLASHCHAR), powers),
    powers: ($) =>
      seq(optional(choice(PLUSCHAR, MINUSCHAR)), basis, repeat(power)),
    power: ($) => seq(DOUBLEASTERISKCHAR, basis),
    basis: ($) =>
      seq(
        LPARENCHAR,
        arithmeticExpression,
        choice(RPARENCHAR, identifier, literal)
      ),
    // condition ----------------------------------
    condition: ($) => seq(combinableCondition, repeat(andOrCondition)),
    andOrCondition: ($) =>
      seq(choice(AND, OR), choice(combinableCondition, repeat1(abbreviation))),
    combinableCondition: ($) => seq(optional(NOT), simpleCondition),
    simpleCondition: ($) =>
      seq(
        LPARENCHAR,
        condition,
        choice(
          RPARENCHAR,
          relationCondition,
          classCondition,
          conditionNameReference
        )
      ),
    classCondition: ($) =>
      seq(
        identifier,
        optional(IS),
        optional(NOT),
        choice(
          NUMERIC,
          ALPHABETIC,
          ALPHABETIC_LOWER,
          ALPHABETIC_UPPER,
          DBCS,
          KANJI,
          className
        )
      ),
    conditionNameReference: ($) =>
      seq(
        conditionName,
        choice(
          seq(
            repeat(inData),
            optional(inFile),
            repeat(conditionNameSubscriptReference)
          ),
          repeat(inMnemonic)
        )
      ),
    conditionNameSubscriptReference: ($) =>
      seq(
        LPARENCHAR,
        subscript,
        repeat(seq(optional(COMMACHAR), subscript)),
        RPARENCHAR
      ),
    // relation ----------------------------------
    relationCondition: ($) =>
      choice(
        relationSignCondition,
        relationArithmeticComparison,
        relationCombinedComparison
      ),
    relationSignCondition: ($) =>
      seq(
        arithmeticExpression,
        optional(IS),
        optional(NOT),
        choice(POSITIVE, NEGATIVE, ZERO)
      ),
    relationArithmeticComparison: ($) =>
      seq(arithmeticExpression, relationalOperator, arithmeticExpression),
    relationCombinedComparison: ($) =>
      seq(
        arithmeticExpression,
        relationalOperator,
        LPARENCHAR,
        relationCombinedCondition,
        RPARENCHAR
      ),
    relationCombinedCondition: ($) =>
      seq(
        arithmeticExpression,
        repeat1(seq(choice(AND, OR), arithmeticExpression))
      ),
    relationalOperator: ($) =>
      seq(
        optional(choice(IS, ARE)),
        choice(
          seq(
            optional(NOT),
            choice(
              seq(GREATER, optional(THAN)),
              MORETHANCHAR,
              seq(LESS, optional(THAN)),
              LESSTHANCHAR,
              seq(EQUAL, optional(TO)),
              EQUALCHAR
            )
          ),
          NOTEQUALCHAR,
          seq(GREATER, optional(THAN), OR, EQUAL, optional(TO)),
          MORETHANOREQUAL,
          seq(LESS, optional(THAN), OR, EQUAL, optional(TO)),
          LESSTHANOREQUAL
        )
      ),
    abbreviation: ($) =>
      seq(
        optional(NOT),
        optional(relationalOperator),
        choice(
          arithmeticExpression,
          seq(LPARENCHAR, arithmeticExpression, abbreviation, RPARENCHAR)
        )
      ),
    // identifier ----------------------------------
    identifier: ($) =>
      choice(qualifiedDataName, tableCall, functionCall, specialRegister),
    tableCall: ($) =>
      seq(
        qualifiedDataName,
        repeat(
          seq(
            LPARENCHAR,
            subscript,
            repeat(seq(optional(COMMACHAR), subscript)),
            RPARENCHAR
          )
        ),
        optional(referenceModifier)
      ),
    functionCall: ($) =>
      seq(
        FUNCTION,
        functionName,
        repeat(
          seq(
            LPARENCHAR,
            argument,
            repeat(seq(optional(COMMACHAR), argument)),
            RPARENCHAR
          )
        ),
        optional(referenceModifier)
      ),
    referenceModifier: ($) =>
      seq(
        LPARENCHAR,
        characterPosition,
        COLONCHAR,
        optional(length),
        RPARENCHAR
      ),
    characterPosition: ($) => arithmeticExpression,
    length: ($) => arithmeticExpression,
    subscript: ($) =>
      seq(
        choice(ALL, integerLiteral, qualifiedDataName),
        choice(optional(integerLiteral), indexName),
        choice(optional(integerLiteral), arithmeticExpression)
      ),
    argument: ($) =>
      seq(
        choice(literal, identifier, qualifiedDataName),
        choice(optional(integerLiteral), indexName),
        choice(optional(integerLiteral), arithmeticExpression)
      ),
    // qualified data name ----------------------------------
    qualifiedDataName: ($) =>
      choice(
        qualifiedDataNameFormat1,
        qualifiedDataNameFormat2,
        qualifiedDataNameFormat3,
        qualifiedDataNameFormat4
      ),
    qualifiedDataNameFormat1: ($) =>
      seq(
        choice(dataName, conditionName),
        optional(
          choice(seq(repeat1(qualifiedInData), optional(inFile)), inFile)
        )
      ),
    qualifiedDataNameFormat2: ($) => seq(paragraphName, inSection),
    qualifiedDataNameFormat3: ($) => seq(textName, inLibrary),
    qualifiedDataNameFormat4: ($) => seq(LINAGE_COUNTER, inFile),
    qualifiedInData: ($) => choice(inData, inTable),
    // in ----------------------------------
    inData: ($) => seq(choice(IN, OF), dataName),
    inFile: ($) => seq(choice(IN, OF), fileName),
    inMnemonic: ($) => seq(choice(IN, OF), mnemonicName),
    inSection: ($) => seq(choice(IN, OF), sectionName),
    inLibrary: ($) => seq(choice(IN, OF), libraryName),
    inTable: ($) => seq(choice(IN, OF), tableCall),
    // names ----------------------------------
    alphabetName: ($) => cobolWord,
    assignmentName: ($) => systemName,
    basisName: ($) => programName,
    cdName: ($) => cobolWord,
    className: ($) => cobolWord,
    computerName: ($) => systemName,
    conditionName: ($) => cobolWord,
    dataName: ($) => cobolWord,
    dataDescName: ($) => choice(FILLER, CURSOR, dataName),
    environmentName: ($) => systemName,
    fileName: ($) => cobolWord,
    functionName: ($) =>
      choice(INTEGER, LENGTH, RANDOM, SUM, WHEN_COMPILED, cobolWord),
    indexName: ($) => cobolWord,
    languageName: ($) => systemName,
    libraryName: ($) => cobolWord,
    localName: ($) => cobolWord,
    mnemonicName: ($) => cobolWord,
    paragraphName: ($) => choice(cobolWord, integerLiteral),
    procedureName: ($) =>
      seq(paragraphName, choice(optional(inSection), sectionName)),
    programName: ($) => choice(NONNUMERICLITERAL, cobolWord),
    recordName: ($) => qualifiedDataName,
    reportName: ($) => qualifiedDataName,
    routineName: ($) => cobolWord,
    screenName: ($) => cobolWord,
    sectionName: ($) => choice(cobolWord, integerLiteral),
    systemName: ($) => cobolWord,
    symbolicCharacter: ($) => cobolWord,
    textName: ($) => cobolWord,
    // literal ----------------------------------
    cobolWord: ($) =>
      choice(
        IDENTIFIER,
        ABORT,
        AS,
        ASCII,
        ASSOCIATED_DATA,
        ASSOCIATED_DATA_LENGTH,
        ATTRIBUTE,
        AUTO,
        AUTO_SKIP,
        BACKGROUND_COLOR,
        BACKGROUND_COLOUR,
        BEEP,
        BELL,
        BINARY,
        BIT,
        BLINK,
        BLOB,
        BOUNDS,
        CAPABLE,
        CCSVERSION,
        CHANGED,
        CHANNEL,
        CLOB,
        CLOSE_DISPOSITION,
        COBOL,
        COMMITMENT,
        CONTROL_POINT,
        CONVENTION,
        CRUNCH,
        CURSOR,
        DBCLOB,
        DEFAULT,
        DEFAULT_DISPLAY,
        DEFINITION,
        DFHRESP,
        DFHVALUE,
        DISK,
        DONTCARE,
        DOUBLE,
        EBCDIC,
        EMPTY_CHECK,
        ENTER,
        ENTRY_PROCEDURE,
        EOL,
        EOS,
        ERASE,
        ESCAPE,
        EVENT,
        EXCLUSIVE,
        EXPORT,
        EXTENDED,
        FOREGROUND_COLOR,
        FOREGROUND_COLOUR,
        FULL,
        FUNCTIONNAME,
        FUNCTION_POINTER,
        GRID,
        HIGHLIGHT,
        IMPLICIT,
        IMPORT,
        INTEGER,
        KEPT,
        KEYBOARD,
        LANGUAGE,
        LB,
        LD,
        LEFTLINE,
        LENGTH_CHECK,
        LIBACCESS,
        LIBPARAMETER,
        LIBRARY,
        LIST,
        LOCAL,
        LONG_DATE,
        LONG_TIME,
        LOWER,
        LOWLIGHT,
        MMDDYYYY,
        NAMED,
        NATIONAL,
        NATIONAL_EDITED,
        NETWORK,
        NO_ECHO,
        NUMERIC_DATE,
        NUMERIC_TIME,
        ODT,
        ORDERLY,
        OVERLINE,
        OWN,
        PASSWORD,
        PORT,
        PRINTER,
        PRIVATE,
        PROCESS,
        PROGRAM,
        PROMPT,
        READER,
        REAL,
        RECEIVED,
        RECURSIVE,
        REF,
        REMOTE,
        REMOVE,
        REQUIRED,
        REVERSE_VIDEO,
        SAVE,
        SECURE,
        SHARED,
        SHAREDBYALL,
        SHAREDBYRUNUNIT,
        SHARING,
        SHORT_DATE,
        SQL,
        SYMBOL,
        TASK,
        THREAD,
        THREAD_LOCAL,
        TIMER,
        TODAYS_DATE,
        TODAYS_NAME,
        TRUNCATED,
        TYPEDEF,
        UNDERLINE,
        VIRTUAL,
        WAIT,
        YEAR,
        YYYYMMDD,
        YYYYDDD,
        ZERO_FILL
      ),
    literal: ($) =>
      choice(
        NONNUMERICLITERAL,
        figurativeConstant,
        numericLiteral,
        booleanLiteral,
        cicsDfhRespLiteral,
        cicsDfhValueLiteral
      ),
    booleanLiteral: ($) => choice(TRUE, FALSE),
    numericLiteral: ($) => choice(NUMERICLITERAL, ZERO, integerLiteral),
    integerLiteral: ($) =>
      choice(INTEGERLITERAL, LEVEL_NUMBER_66, LEVEL_NUMBER_77, LEVEL_NUMBER_88),
    cicsDfhRespLiteral: ($) =>
      seq(DFHRESP, LPARENCHAR, choice(cobolWord, literal), RPARENCHAR),
    cicsDfhValueLiteral: ($) =>
      seq(DFHVALUE, LPARENCHAR, choice(cobolWord, literal), RPARENCHAR),
    // keywords ----------------------------------
    figurativeConstant: ($) =>
      seq(
        ALL,
        choice(
          literal,
          HIGH_VALUE,
          HIGH_VALUES,
          LOW_VALUE,
          LOW_VALUES,
          NULL,
          NULLS,
          QUOTE,
          QUOTES,
          SPACE,
          SPACES,
          ZERO,
          ZEROS,
          ZEROES
        )
      ),
    specialRegister: ($) =>
      seq(
        ADDRESS,
        OF,
        choice(
          identifier,
          DATE,
          DAY,
          DAY_OF_WEEK,
          DEBUG_CONTENTS,
          DEBUG_ITEM,
          DEBUG_LINE,
          DEBUG_NAME,
          DEBUG_SUB_1,
          DEBUG_SUB_2,
          DEBUG_SUB_3,
          LENGTH
        ),
        optional(OF),
        choice(
          identifier,
          LINAGE_COUNTER,
          LINE_COUNTER,
          PAGE_COUNTER,
          RETURN_CODE,
          SHIFT_IN,
          SHIFT_OUT,
          SORT_CONTROL,
          SORT_CORE_SIZE,
          SORT_FILE_SIZE,
          SORT_MESSAGE,
          SORT_MODE_SIZE,
          SORT_RETURN,
          TALLY,
          TIME,
          WHEN_COMPILED
        )
      ),
    // comment entry
    commentEntry: ($) => repeat1(COMMENTENTRYLINE),
    // lexer rules --------------------------------------------------------------------------------
    // keywords
    ABORT: ($) => seq(A, B, O, R, T),
    ACCEPT: ($) => seq(A, C, C, E, P, T),
    ACCESS: ($) => seq(A, C, C, E, S, S),
    ADD: ($) => seq(A, D, D),
    ADDRESS: ($) => seq(A, D, D, R, E, S, S),
    ADVANCING: ($) => seq(A, D, V, A, N, C, I, N, G),
    AFTER: ($) => seq(A, F, T, E, R),
    ALIGNED: ($) => seq(A, L, I, G, N, E, D),
    ALL: ($) => seq(A, L, L),
    ALPHABET: ($) => seq(A, L, P, H, A, B, E, T),
    ALPHABETIC: ($) => seq(A, L, P, H, A, B, E, T, I, C),
    ALPHABETIC_LOWER: ($) =>
      seq(A, L, P, H, A, B, E, T, I, C, MINUSCHAR, L, O, W, E, R),
    ALPHABETIC_UPPER: ($) =>
      seq(A, L, P, H, A, B, E, T, I, C, MINUSCHAR, U, P, P, E, R),
    ALPHANUMERIC: ($) => seq(A, L, P, H, A, N, U, M, E, R, I, C),
    ALPHANUMERIC_EDITED: ($) =>
      seq(A, L, P, H, A, N, U, M, E, R, I, C, MINUSCHAR, E, D, I, T, E, D),
    ALSO: ($) => seq(A, L, S, O),
    ALTER: ($) => seq(A, L, T, E, R),
    ALTERNATE: ($) => seq(A, L, T, E, R, N, A, T, E),
    AND: ($) => seq(A, N, D),
    ANY: ($) => seq(A, N, Y),
    ARE: ($) => seq(A, R, E),
    AREA: ($) => seq(A, R, E, A),
    AREAS: ($) => seq(A, R, E, A, S),
    AS: ($) => seq(A, S),
    ASCENDING: ($) => seq(A, S, C, E, N, D, I, N, G),
    ASCII: ($) => seq(A, S, C, I, I),
    ASSIGN: ($) => seq(A, S, S, I, G, N),
    ASSOCIATED_DATA: ($) =>
      seq(A, S, S, O, C, I, A, T, E, D, MINUSCHAR, D, A, T, A),
    ASSOCIATED_DATA_LENGTH: ($) =>
      seq(
        A,
        S,
        S,
        O,
        C,
        I,
        A,
        T,
        E,
        D,
        MINUSCHAR,
        D,
        A,
        T,
        A,
        MINUSCHAR,
        L,
        E,
        N,
        G,
        T,
        H
      ),
    AT: ($) => seq(A, T),
    ATTRIBUTE: ($) => seq(A, T, T, R, I, B, U, T, E),
    AUTHOR: ($) => seq(A, U, T, H, O, R),
    AUTO: ($) => seq(A, U, T, O),
    AUTO_SKIP: ($) => seq(A, U, T, O, MINUSCHAR, S, K, I, P),
    BACKGROUND_COLOR: ($) =>
      seq(B, A, C, K, G, R, O, U, N, D, MINUSCHAR, C, O, L, O, R),
    BACKGROUND_COLOUR: ($) =>
      seq(B, A, C, K, G, R, O, U, N, D, MINUSCHAR, C, O, L, O, U, R),
    BASIS: ($) => seq(B, A, S, I, S),
    BEEP: ($) => seq(B, E, E, P),
    BEFORE: ($) => seq(B, E, F, O, R, E),
    BEGINNING: ($) => seq(B, E, G, I, N, N, I, N, G),
    BELL: ($) => seq(B, E, L, L),
    BINARY: ($) => seq(B, I, N, A, R, Y),
    BIT: ($) => seq(B, I, T),
    BLANK: ($) => seq(B, L, A, N, K),
    BLINK: ($) => seq(B, L, I, N, K),
    BLOB: ($) => seq(B, L, O, B),
    BLOCK: ($) => seq(B, L, O, C, K),
    BOUNDS: ($) => seq(B, O, U, N, D, S),
    BOTTOM: ($) => seq(B, O, T, T, O, M),
    BY: ($) => seq(B, Y),
    BYFUNCTION: ($) => seq(B, Y, F, U, N, C, T, I, O, N),
    BYTITLE: ($) => seq(B, Y, T, I, T, L, E),
    CALL: ($) => seq(C, A, L, L),
    CANCEL: ($) => seq(C, A, N, C, E, L),
    CAPABLE: ($) => seq(C, A, P, A, B, L, E),
    CCSVERSION: ($) => seq(C, C, S, V, E, R, S, I, O, N),
    CD: ($) => seq(C, D),
    CF: ($) => seq(C, F),
    CH: ($) => seq(C, H),
    CHAINING: ($) => seq(C, H, A, I, N, I, N, G),
    CHANGED: ($) => seq(C, H, A, N, G, E, D),
    CHANNEL: ($) => seq(C, H, A, N, N, E, L),
    CHARACTER: ($) => seq(C, H, A, R, A, C, T, E, R),
    CHARACTERS: ($) => seq(C, H, A, R, A, C, T, E, R, S),
    CLASS: ($) => seq(C, L, A, S, S),
    CLASS_ID: ($) => seq(C, L, A, S, S, MINUSCHAR, I, D),
    CLOB: ($) => seq(C, L, O, B),
    CLOCK_UNITS: ($) => seq(C, L, O, C, K, MINUSCHAR, U, N, I, T, S),
    CLOSE: ($) => seq(C, L, O, S, E),
    CLOSE_DISPOSITION: ($) =>
      seq(C, L, O, S, E, MINUSCHAR, D, I, S, P, O, S, I, T, I, O, N),
    COBOL: ($) => seq(C, O, B, O, L),
    CODE: ($) => seq(C, O, D, E),
    CODE_SET: ($) => seq(C, O, D, E, MINUSCHAR, S, E, T),
    COLLATING: ($) => seq(C, O, L, L, A, T, I, N, G),
    COL: ($) => seq(C, O, L),
    COLUMN: ($) => seq(C, O, L, U, M, N),
    COM_REG: ($) => seq(C, O, M, MINUSCHAR, R, E, G),
    COMMA: ($) => seq(C, O, M, M, A),
    COMMITMENT: ($) => seq(C, O, M, M, I, T, M, E, N, T),
    COMMON: ($) => seq(C, O, M, M, O, N),
    COMMUNICATION: ($) => seq(C, O, M, M, U, N, I, C, A, T, I, O, N),
    COMP: ($) => seq(C, O, M, P),
    COMP_1: ($) => seq(C, O, M, P, MINUSCHAR, "1"),
    COMP_2: ($) => seq(C, O, M, P, MINUSCHAR, "2"),
    COMP_3: ($) => seq(C, O, M, P, MINUSCHAR, "3"),
    COMP_4: ($) => seq(C, O, M, P, MINUSCHAR, "4"),
    COMP_5: ($) => seq(C, O, M, P, MINUSCHAR, "5"),
    COMPUTATIONAL: ($) => seq(C, O, M, P, U, T, A, T, I, O, N, A, L),
    COMPUTATIONAL_1: ($) =>
      seq(C, O, M, P, U, T, A, T, I, O, N, A, L, MINUSCHAR, "1"),
    COMPUTATIONAL_2: ($) =>
      seq(C, O, M, P, U, T, A, T, I, O, N, A, L, MINUSCHAR, "2"),
    COMPUTATIONAL_3: ($) =>
      seq(C, O, M, P, U, T, A, T, I, O, N, A, L, MINUSCHAR, "3"),
    COMPUTATIONAL_4: ($) =>
      seq(C, O, M, P, U, T, A, T, I, O, N, A, L, MINUSCHAR, "4"),
    COMPUTATIONAL_5: ($) =>
      seq(C, O, M, P, U, T, A, T, I, O, N, A, L, MINUSCHAR, "5"),
    COMPUTE: ($) => seq(C, O, M, P, U, T, E),
    CONFIGURATION: ($) => seq(C, O, N, F, I, G, U, R, A, T, I, O, N),
    CONTAINS: ($) => seq(C, O, N, T, A, I, N, S),
    CONTENT: ($) => seq(C, O, N, T, E, N, T),
    CONTINUE: ($) => seq(C, O, N, T, I, N, U, E),
    CONTROL: ($) => seq(C, O, N, T, R, O, L),
    CONTROL_POINT: ($) => seq(C, O, N, T, R, O, L, MINUSCHAR, P, O, I, N, T),
    CONTROLS: ($) => seq(C, O, N, T, R, O, L, S),
    CONVENTION: ($) => seq(C, O, N, V, E, N, T, I, O, N),
    CONVERTING: ($) => seq(C, O, N, V, E, R, T, I, N, G),
    COPY: ($) => seq(C, O, P, Y),
    CORR: ($) => seq(C, O, R, R),
    CORRESPONDING: ($) => seq(C, O, R, R, E, S, P, O, N, D, I, N, G),
    COUNT: ($) => seq(C, O, U, N, T),
    CRUNCH: ($) => seq(C, R, U, N, C, H),
    CURRENCY: ($) => seq(C, U, R, R, E, N, C, Y),
    CURSOR: ($) => seq(C, U, R, S, O, R),
    DATA: ($) => seq(D, A, T, A),
    DATA_BASE: ($) => seq(D, A, T, A, MINUSCHAR, B, A, S, E),
    DATE: ($) => seq(D, A, T, E),
    DATE_COMPILED: ($) => seq(D, A, T, E, MINUSCHAR, C, O, M, P, I, L, E, D),
    DATE_WRITTEN: ($) => seq(D, A, T, E, MINUSCHAR, W, R, I, T, T, E, N),
    DAY: ($) => seq(D, A, Y),
    DAY_OF_WEEK: ($) => seq(D, A, Y, MINUSCHAR, O, F, MINUSCHAR, W, E, E, K),
    DBCS: ($) => seq(D, B, C, S),
    DBCLOB: ($) => seq(D, B, C, L, O, B),
    DE: ($) => seq(D, E),
    DEBUG_CONTENTS: ($) =>
      seq(D, E, B, U, G, MINUSCHAR, C, O, N, T, E, N, T, S),
    DEBUG_ITEM: ($) => seq(D, E, B, U, G, MINUSCHAR, I, T, E, M),
    DEBUG_LINE: ($) => seq(D, E, B, U, G, MINUSCHAR, L, I, N, E),
    DEBUG_NAME: ($) => seq(D, E, B, U, G, MINUSCHAR, N, A, M, E),
    DEBUG_SUB_1: ($) => seq(D, E, B, U, G, MINUSCHAR, S, U, B, MINUSCHAR, "1"),
    DEBUG_SUB_2: ($) => seq(D, E, B, U, G, MINUSCHAR, S, U, B, MINUSCHAR, "2"),
    DEBUG_SUB_3: ($) => seq(D, E, B, U, G, MINUSCHAR, S, U, B, MINUSCHAR, "3"),
    DEBUGGING: ($) => seq(D, E, B, U, G, G, I, N, G),
    DECIMAL_POINT: ($) => seq(D, E, C, I, M, A, L, MINUSCHAR, P, O, I, N, T),
    DECLARATIVES: ($) => seq(D, E, C, L, A, R, A, T, I, V, E, S),
    DEFAULT: ($) => seq(D, E, F, A, U, L, T),
    DEFAULT_DISPLAY: ($) =>
      seq(D, E, F, A, U, L, T, MINUSCHAR, D, I, S, P, L, A, Y),
    DEFINITION: ($) => seq(D, E, F, I, N, I, T, I, O, N),
    DELETE: ($) => seq(D, E, L, E, T, E),
    DELIMITED: ($) => seq(D, E, L, I, M, I, T, E, D),
    DELIMITER: ($) => seq(D, E, L, I, M, I, T, E, R),
    DEPENDING: ($) => seq(D, E, P, E, N, D, I, N, G),
    DESCENDING: ($) => seq(D, E, S, C, E, N, D, I, N, G),
    DESTINATION: ($) => seq(D, E, S, T, I, N, A, T, I, O, N),
    DETAIL: ($) => seq(D, E, T, A, I, L),
    DFHRESP: ($) => seq(D, F, H, R, E, S, P),
    DFHVALUE: ($) => seq(D, F, H, V, A, L, U, E),
    DISABLE: ($) => seq(D, I, S, A, B, L, E),
    DISK: ($) => seq(D, I, S, K),
    DISPLAY: ($) => seq(D, I, S, P, L, A, Y),
    DISPLAY_1: ($) => seq(D, I, S, P, L, A, Y, MINUSCHAR, "1"),
    DIVIDE: ($) => seq(D, I, V, I, D, E),
    DIVISION: ($) => seq(D, I, V, I, S, I, O, N),
    DONTCARE: ($) => seq(D, O, N, T, C, A, R, E),
    DOUBLE: ($) => seq(D, O, U, B, L, E),
    DOWN: ($) => seq(D, O, W, N),
    DUPLICATES: ($) => seq(D, U, P, L, I, C, A, T, E, S),
    DYNAMIC: ($) => seq(D, Y, N, A, M, I, C),
    EBCDIC: ($) => seq(E, B, C, D, I, C),
    EGCS: ($) => seq(E, G, C, S), // E, X, T, E, N, S, I, O, N
    EGI: ($) => seq(E, G, I),
    ELSE: ($) => seq(E, L, S, E),
    EMI: ($) => seq(E, M, I),
    EMPTY_CHECK: ($) => seq(E, M, P, T, Y, MINUSCHAR, C, H, E, C, K),
    ENABLE: ($) => seq(E, N, A, B, L, E),
    END: ($) => seq(E, N, D),
    END_ACCEPT: ($) => seq(E, N, D, MINUSCHAR, A, C, C, E, P, T),
    END_ADD: ($) => seq(E, N, D, MINUSCHAR, A, D, D),
    END_CALL: ($) => seq(E, N, D, MINUSCHAR, C, A, L, L),
    END_COMPUTE: ($) => seq(E, N, D, MINUSCHAR, C, O, M, P, U, T, E),
    END_DELETE: ($) => seq(E, N, D, MINUSCHAR, D, E, L, E, T, E),
    END_DISPLAY: ($) => seq(E, N, D, MINUSCHAR, D, I, S, P, L, A, Y),
    END_DIVIDE: ($) => seq(E, N, D, MINUSCHAR, D, I, V, I, D, E),
    END_EVALUATE: ($) => seq(E, N, D, MINUSCHAR, E, V, A, L, U, A, T, E),
    END_IF: ($) => seq(E, N, D, MINUSCHAR, I, F),
    END_MULTIPLY: ($) => seq(E, N, D, MINUSCHAR, M, U, L, T, I, P, L, Y),
    END_OF_PAGE: ($) => seq(E, N, D, MINUSCHAR, O, F, MINUSCHAR, P, A, G, E),
    END_PERFORM: ($) => seq(E, N, D, MINUSCHAR, P, E, R, F, O, R, M),
    END_READ: ($) => seq(E, N, D, MINUSCHAR, R, E, A, D),
    END_RECEIVE: ($) => seq(E, N, D, MINUSCHAR, R, E, C, E, I, V, E),
    END_REMARKS: ($) => seq(E, N, D, MINUSCHAR, R, E, M, A, R, K, S),
    END_RETURN: ($) => seq(E, N, D, MINUSCHAR, R, E, T, U, R, N),
    END_REWRITE: ($) => seq(E, N, D, MINUSCHAR, R, E, W, R, I, T, E),
    END_SEARCH: ($) => seq(E, N, D, MINUSCHAR, S, E, A, R, C, H),
    END_START: ($) => seq(E, N, D, MINUSCHAR, S, T, A, R, T),
    END_STRING: ($) => seq(E, N, D, MINUSCHAR, S, T, R, I, N, G),
    END_SUBTRACT: ($) => seq(E, N, D, MINUSCHAR, S, U, B, T, R, A, C, T),
    END_UNSTRING: ($) => seq(E, N, D, MINUSCHAR, U, N, S, T, R, I, N, G),
    END_WRITE: ($) => seq(E, N, D, MINUSCHAR, W, R, I, T, E),
    ENDING: ($) => seq(E, N, D, I, N, F),
    ENTER: ($) => seq(E, N, T, E, R),
    ENTRY: ($) => seq(E, N, T, R, Y),
    ENTRY_PROCEDURE: ($) =>
      seq(E, N, T, R, Y, MINUSCHAR, P, R, O, C, E, D, U, R, E),
    ENVIRONMENT: ($) => seq(E, N, V, I, R, O, N, M, E, N, T),
    EOP: ($) => seq(E, O, P),
    EQUAL: ($) => seq(E, Q, U, A, L),
    ERASE: ($) => seq(E, R, A, S, E),
    ERROR: ($) => seq(E, R, R, O, R),
    EOL: ($) => seq(E, O, L),
    EOS: ($) => seq(E, O, S),
    ESCAPE: ($) => seq(E, S, C, A, P, E),
    ESI: ($) => seq(E, S, I),
    EVALUATE: ($) => seq(E, V, A, L, U, A, T, E),
    EVENT: ($) => seq(E, V, E, N, T),
    EVERY: ($) => seq(E, V, E, R, Y),
    EXCEPTION: ($) => seq(E, X, C, E, P, T, I, O, N),
    EXCLUSIVE: ($) => seq(E, X, C, L, U, S, I, V, E),
    EXHIBIT: ($) => seq(E, X, H, I, B, I, T),
    EXIT: ($) => seq(E, X, I, T),
    EXPORT: ($) => seq(E, X, P, O, R, T),
    EXTEND: ($) => seq(E, X, T, E, N, D),
    EXTENDED: ($) => seq(E, X, T, E, N, D, E, D),
    EXTERNAL: ($) => seq(E, X, T, E, R, N, A, L),
    FALSE: ($) => seq(F, A, L, S, E),
    FD: ($) => seq(F, D),
    FILE: ($) => seq(F, I, L, E),
    FILE_CONTROL: ($) => seq(F, I, L, E, MINUSCHAR, C, O, N, T, R, O, L),
    FILLER: ($) => seq(F, I, L, L, E, R),
    FINAL: ($) => seq(F, I, N, A, L),
    FIRST: ($) => seq(F, I, R, S, T),
    FOOTING: ($) => seq(F, O, O, T, I, N, G),
    FOR: ($) => seq(F, O, R),
    FOREGROUND_COLOR: ($) =>
      seq(F, O, R, E, G, R, O, U, N, D, MINUSCHAR, C, O, L, O, R),
    FOREGROUND_COLOUR: ($) =>
      seq(F, O, R, E, G, R, O, U, N, D, MINUSCHAR, C, O, L, O, U, R),
    FROM: ($) => seq(F, R, O, M),
    FULL: ($) => seq(F, U, L, L),
    FUNCTION: ($) => seq(F, U, N, C, T, I, O, N),
    FUNCTIONNAME: ($) => seq(F, U, N, C, T, I, O, N, N, A, M, E),
    FUNCTION_POINTER: ($) =>
      seq(F, U, N, C, T, I, O, N, MINUSCHAR, P, O, I, N, T, E, R),
    GENERATE: ($) => seq(G, E, N, E, R, A, T, E),
    GOBACK: ($) => seq(G, O, B, A, C, K),
    GIVING: ($) => seq(G, I, V, I, N, G),
    GLOBAL: ($) => seq(G, L, O, B, A, L),
    GO: ($) => seq(G, O),
    GREATER: ($) => seq(G, R, E, A, T, E, R),
    GRID: ($) => seq(G, R, I, D),
    GROUP: ($) => seq(G, R, O, U, P),
    HEADING: ($) => seq(H, E, A, D, I, N, G),
    HIGHLIGHT: ($) => seq(H, I, G, H, L, I, G, H, T),
    HIGH_VALUE: ($) => seq(H, I, G, H, MINUSCHAR, V, A, L, U, E),
    HIGH_VALUES: ($) => seq(H, I, G, H, MINUSCHAR, V, A, L, U, E, S),
    I_O: ($) => seq(I, MINUSCHAR, O),
    I_O_CONTROL: ($) => seq(I, MINUSCHAR, O, MINUSCHAR, C, O, N, T, R, O, L),
    ID: ($) => seq(I, D),
    IDENTIFICATION: ($) => seq(I, D, E, N, T, I, F, I, C, A, T, I, O, N),
    IF: ($) => seq(I, F),
    IMPLICIT: ($) => seq(I, M, P, L, I, C, I, T),
    IMPORT: ($) => seq(I, M, P, O, R, T),
    IN: ($) => seq(I, N),
    INDEX: ($) => seq(I, N, D, E, X),
    INDEXED: ($) => seq(I, N, D, E, X, E, D),
    INDICATE: ($) => seq(I, N, D, I, C, A, T, E),
    INITIAL: ($) => seq(I, N, I, T, I, A, L),
    INITIALIZE: ($) => seq(I, N, I, T, I, A, L, I, Z, E),
    INITIATE: ($) => seq(I, N, I, T, I, A, T, E),
    INPUT: ($) => seq(I, N, P, U, T),
    INPUT_OUTPUT: ($) => seq(I, N, P, U, T, MINUSCHAR, O, U, T, P, U, T),
    INSPECT: ($) => seq(I, N, S, P, E, C, T),
    INSTALLATION: ($) => seq(I, N, S, T, A, L, L, A, T, I, O, N),
    INTEGER: ($) => seq(I, N, T, E, G, E, R),
    INTO: ($) => seq(I, N, T, O),
    INVALID: ($) => seq(I, N, V, A, L, I, D),
    INVOKE: ($) => seq(I, N, V, O, K, E),
    IS: ($) => seq(I, S),
    JUST: ($) => seq(J, U, S, T),
    JUSTIFIED: ($) => seq(J, U, S, T, I, F, I, E, D),
    KANJI: ($) => seq(K, A, N, J, I),
    KEPT: ($) => seq(K, E, P, T),
    KEY: ($) => seq(K, E, Y),
    KEYBOARD: ($) => seq(K, E, Y, B, O, A, R, D),
    LABEL: ($) => seq(L, A, B, E, L),
    LANGUAGE: ($) => seq(L, A, N, G, U, A, G, E),
    LAST: ($) => seq(L, A, S, T),
    LB: ($) => seq(L, B),
    LD: ($) => seq(L, D),
    LEADING: ($) => seq(L, E, A, D, I, N, G),
    LEFT: ($) => seq(L, E, F, T),
    LEFTLINE: ($) => seq(L, E, F, T, L, I, N, E),
    LENGTH: ($) => seq(L, E, N, G, T, H),
    LENGTH_CHECK: ($) => seq(L, E, N, G, T, H, MINUSCHAR, C, H, E, C, K),
    LESS: ($) => seq(L, E, S, S),
    LIBACCESS: ($) => seq(L, I, B, A, C, C, E, S, S),
    LIBPARAMETER: ($) => seq(L, I, B, P, A, R, A, M, E, T, E, R),
    LIBRARY: ($) => seq(L, I, B, R, A, R, Y),
    LIMIT: ($) => seq(L, I, M, I, T),
    LIMITS: ($) => seq(L, I, M, I, T, S),
    LINAGE: ($) => seq(L, I, N, A, G, E),
    LINAGE_COUNTER: ($) =>
      seq(L, I, N, A, G, E, MINUSCHAR, C, O, U, N, T, E, R),
    LINE: ($) => seq(L, I, N, E),
    LINES: ($) => seq(L, I, N, E, S),
    LINE_COUNTER: ($) => seq(L, I, N, E, MINUSCHAR, C, O, U, N, T, E, R),
    LINKAGE: ($) => seq(L, I, N, K, A, G, E),
    LIST: ($) => seq(L, I, S, T),
    LOCAL: ($) => seq(L, O, C, A, L),
    LOCAL_STORAGE: ($) => seq(L, O, C, A, L, MINUSCHAR, S, T, O, R, A, G, E),
    LOCK: ($) => seq(L, O, C, K),
    LONG_DATE: ($) => seq(L, O, N, G, MINUSCHAR, D, A, T, E),
    LONG_TIME: ($) => seq(L, O, N, G, MINUSCHAR, T, I, M, E),
    LOWER: ($) => seq(L, O, W, E, R),
    LOWLIGHT: ($) => seq(L, O, W, L, I, G, H, T),
    LOW_VALUE: ($) => seq(L, O, W, MINUSCHAR, V, A, L, U, E),
    LOW_VALUES: ($) => seq(L, O, W, MINUSCHAR, V, A, L, U, E, S),
    MEMORY: ($) => seq(M, E, M, O, R, Y),
    MERGE: ($) => seq(M, E, R, G, E),
    MESSAGE: ($) => seq(M, E, S, S, A, G, E),
    MMDDYYYY: ($) => seq(M, M, D, D, Y, Y, Y, Y),
    MODE: ($) => seq(M, O, D, E),
    MODULES: ($) => seq(M, O, D, U, L, E, S),
    MORE_LABELS: ($) => seq(M, O, R, E, MINUSCHAR, L, A, B, E, L, S),
    MOVE: ($) => seq(M, O, V, E),
    MULTIPLE: ($) => seq(M, U, L, T, I, P, L, E),
    MULTIPLY: ($) => seq(M, U, L, T, I, P, L, Y),
    NAMED: ($) => seq(N, A, M, E, D),
    NATIONAL: ($) => seq(N, A, T, I, O, N, A, L),
    NATIONAL_EDITED: ($) =>
      seq(N, A, T, I, O, N, A, L, MINUSCHAR, E, D, I, T, E, D),
    NATIVE: ($) => seq(N, A, T, I, V, E),
    NEGATIVE: ($) => seq(N, E, G, A, T, I, V, E),
    NETWORK: ($) => seq(N, E, T, W, O, R, K),
    NEXT: ($) => seq(N, E, X, T),
    NO: ($) => seq(N, O),
    NO_ECHO: ($) => seq(N, O, MINUSCHAR, E, C, H, O),
    NOT: ($) => seq(N, O, T),
    NULL: ($) => seq(N, U, L, L),
    NULLS: ($) => seq(N, U, L, L, S),
    NUMBER: ($) => seq(N, U, M, B, E, R),
    NUMERIC: ($) => seq(N, U, M, E, R, I, C),
    NUMERIC_DATE: ($) => seq(N, U, M, E, R, I, C, MINUSCHAR, D, A, T, E),
    NUMERIC_EDITED: ($) =>
      seq(N, U, M, E, R, I, C, MINUSCHAR, E, D, I, T, E, D),
    NUMERIC_TIME: ($) => seq(N, U, M, E, R, I, C, MINUSCHAR, T, I, M, E),
    OBJECT_COMPUTER: ($) =>
      seq(O, B, J, E, C, T, MINUSCHAR, C, O, M, P, U, T, E, R),
    OCCURS: ($) => seq(O, C, C, U, R, S),
    ODT: ($) => seq(O, D, T),
    OF: ($) => seq(O, F),
    OFF: ($) => seq(O, F, F),
    OMITTED: ($) => seq(O, M, I, T, T, E, D),
    ON: ($) => seq(O, N),
    OPEN: ($) => seq(O, P, E, N),
    OPTIONAL: ($) => seq(O, P, T, I, O, N, A, L),
    OR: ($) => seq(O, R),
    ORDER: ($) => seq(O, R, D, E, R),
    ORDERLY: ($) => seq(O, R, D, E, R, L, Y),
    ORGANIZATION: ($) => seq(O, R, G, A, N, I, Z, A, T, I, O, N),
    OTHER: ($) => seq(O, T, H, E, R),
    OUTPUT: ($) => seq(O, U, T, P, U, T),
    OVERFLOW: ($) => seq(O, V, E, R, F, L, O, W),
    OVERLINE: ($) => seq(O, V, E, R, L, I, N, E),
    OWN: ($) => seq(O, W, N),
    PACKED_DECIMAL: ($) =>
      seq(P, A, C, K, E, D, MINUSCHAR, D, E, C, I, M, A, L),
    PADDING: ($) => seq(P, A, D, D, I, N, G),
    PAGE: ($) => seq(P, A, G, E),
    PAGE_COUNTER: ($) => seq(P, A, G, E, MINUSCHAR, C, O, U, N, T, E, R),
    PASSWORD: ($) => seq(P, A, S, S, W, O, R, D),
    PERFORM: ($) => seq(P, E, R, F, O, R, M),
    PF: ($) => seq(P, F),
    PH: ($) => seq(P, H),
    PIC: ($) => seq(P, I, C),
    PICTURE: ($) => seq(P, I, C, T, U, R, E),
    PLUS: ($) => seq(P, L, U, S),
    POINTER: ($) => seq(P, O, I, N, T, E, R),
    POSITION: ($) => seq(P, O, S, I, T, I, O, N),
    POSITIVE: ($) => seq(P, O, S, I, T, I, V, E),
    PORT: ($) => seq(P, O, R, T),
    PRINTER: ($) => seq(P, R, I, N, T, E, R),
    PRINTING: ($) => seq(P, R, I, N, T, I, N, G),
    PRIVATE: ($) => seq(P, R, I, V, A, T, E),
    PROCEDURE: ($) => seq(P, R, O, C, E, D, U, R, E),
    PROCEDURE_POINTER: ($) =>
      seq(P, R, O, C, E, D, U, R, E, MINUSCHAR, P, O, I, N, T, E, R),
    PROCEDURES: ($) => seq(P, R, O, C, E, D, U, R, E, S),
    PROCEED: ($) => seq(P, R, O, C, E, E, D),
    PROCESS: ($) => seq(P, R, O, C, E, S, S),
    PROGRAM: ($) => seq(P, R, O, G, R, A, M),
    PROGRAM_ID: ($) => seq(P, R, O, G, R, A, M, MINUSCHAR, I, D),
    PROGRAM_LIBRARY: ($) =>
      seq(P, R, O, G, R, A, M, MINUSCHAR, L, I, B, R, A, R, Y),
    PROMPT: ($) => seq(P, R, O, M, P, T),
    PURGE: ($) => seq(P, U, R, G, E),
    QUEUE: ($) => seq(Q, U, E, U, E),
    QUOTE: ($) => seq(Q, U, O, T, E),
    QUOTES: ($) => seq(Q, U, O, T, E, S),
    RANDOM: ($) => seq(R, A, N, D, O, M),
    READER: ($) => seq(R, E, A, D, E, R),
    REMOTE: ($) => seq(R, E, M, O, T, E),
    RD: ($) => seq(R, D),
    REAL: ($) => seq(R, E, A, L),
    READ: ($) => seq(R, E, A, D),
    RECEIVE: ($) => seq(R, E, C, E, I, V, E),
    RECEIVED: ($) => seq(R, E, C, E, I, V, E, D),
    RECORD: ($) => seq(R, E, C, O, R, D),
    RECORDING: ($) => seq(R, E, C, O, R, D, I, N, G),
    RECORDS: ($) => seq(R, E, C, O, R, D, S),
    RECURSIVE: ($) => seq(R, E, C, U, R, S, I, V, E),
    REDEFINES: ($) => seq(R, E, D, E, F, I, N, E, S),
    REEL: ($) => seq(R, E, E, L),
    REF: ($) => seq(R, E, F),
    REFERENCE: ($) => seq(R, E, F, E, R, E, N, C, E),
    REFERENCES: ($) => seq(R, E, F, E, R, E, N, C, E, S),
    RELATIVE: ($) => seq(R, E, L, A, T, I, V, E),
    RELEASE: ($) => seq(R, E, L, E, A, S, E),
    REMAINDER: ($) => seq(R, E, M, A, I, N, D, E, R),
    REMARKS: ($) => seq(R, E, M, A, R, K, S),
    REMOVAL: ($) => seq(R, E, M, O, V, A, L),
    REMOVE: ($) => seq(R, E, M, O, V, E),
    RENAMES: ($) => seq(R, E, N, A, M, E, S),
    REPLACE: ($) => seq(R, E, P, L, A, C, E),
    REPLACING: ($) => seq(R, E, P, L, A, C, I, N, G),
    REPORT: ($) => seq(R, E, P, O, R, T),
    REPORTING: ($) => seq(R, E, P, O, R, T, I, N, G),
    REPORTS: ($) => seq(R, E, P, O, R, T, S),
    REQUIRED: ($) => seq(R, E, Q, U, I, R, E, D),
    RERUN: ($) => seq(R, E, R, U, N),
    RESERVE: ($) => seq(R, E, S, E, R, V, E),
    REVERSE_VIDEO: ($) => seq(R, E, S, E, R, V, E, MINUSCHAR, V, I, D, E, O),
    RESET: ($) => seq(R, E, S, E, T),
    RETURN: ($) => seq(R, E, T, U, R, N),
    RETURN_CODE: ($) => seq(R, E, T, U, R, N, MINUSCHAR, C, O, D, E),
    RETURNING: ($) => seq(R, E, T, U, R, N, I, N, G),
    REVERSED: ($) => seq(R, E, V, E, R, S, E, D),
    REWIND: ($) => seq(R, E, W, I, N, D),
    REWRITE: ($) => seq(R, E, W, R, I, T, E),
    RF: ($) => seq(R, F),
    RH: ($) => seq(R, H),
    RIGHT: ($) => seq(R, I, G, H, T),
    ROUNDED: ($) => seq(R, O, U, N, D, E, D),
    RUN: ($) => seq(R, U, N),
    SAME: ($) => seq(S, A, M, E),
    SAVE: ($) => seq(S, A, V, E),
    SCREEN: ($) => seq(S, C, R, E, E, N),
    SD: ($) => seq(S, D),
    SEARCH: ($) => seq(S, E, A, R, C, H),
    SECTION: ($) => seq(S, E, C, T, I, O, N),
    SECURE: ($) => seq(S, E, C, U, R, E),
    SECURITY: ($) => seq(S, E, C, U, R, I, T, Y),
    SEGMENT: ($) => seq(S, E, G, M, E, N, T),
    SEGMENT_LIMIT: ($) => seq(S, E, G, M, E, N, T, MINUSCHAR, L, I, M, I, T),
    SELECT: ($) => seq(S, E, L, E, C, T),
    SEND: ($) => seq(S, E, N, D),
    SENTENCE: ($) => seq(S, E, N, T, E, N, C, E),
    SEPARATE: ($) => seq(S, E, P, A, R, A, T, E),
    SEQUENCE: ($) => seq(S, E, Q, U, E, N, C, E),
    SEQUENTIAL: ($) => seq(S, E, Q, U, E, N, T, I, A, L),
    SET: ($) => seq(S, E, T),
    SHARED: ($) => seq(S, H, A, R, E, D),
    SHAREDBYALL: ($) => seq(S, H, A, R, E, D, B, Y, A, L, L),
    SHAREDBYRUNUNIT: ($) => seq(S, H, A, R, E, D, B, Y, R, U, N, U, N, I, T),
    SHARING: ($) => seq(S, H, A, R, I, N, G),
    SHIFT_IN: ($) => seq(S, H, I, F, T, MINUSCHAR, I, N),
    SHIFT_OUT: ($) => seq(S, H, I, F, T, MINUSCHAR, O, U, T),
    SHORT_DATE: ($) => seq(S, H, O, R, T, MINUSCHAR, D, A, T, E),
    SIGN: ($) => seq(S, I, G, N),
    SIZE: ($) => seq(S, I, Z, E),
    SORT: ($) => seq(S, O, R, T),
    SORT_CONTROL: ($) => seq(S, O, R, T, MINUSCHAR, C, O, N, T, R, O, L),
    SORT_CORE_SIZE: ($) =>
      seq(S, O, R, T, MINUSCHAR, C, O, R, E, MINUSCHAR, S, I, Z, E),
    SORT_FILE_SIZE: ($) =>
      seq(S, O, R, T, MINUSCHAR, F, I, L, E, MINUSCHAR, S, I, Z, E),
    SORT_MERGE: ($) => seq(S, O, R, T, MINUSCHAR, M, E, R, G, E),
    SORT_MESSAGE: ($) => seq(S, O, R, T, MINUSCHAR, M, E, S, S, A, G, E),
    SORT_MODE_SIZE: ($) =>
      seq(S, O, R, T, MINUSCHAR, M, O, D, E, MINUSCHAR, S, I, Z, E),
    SORT_RETURN: ($) => seq(S, O, R, T, MINUSCHAR, R, E, T, U, R, N),
    SOURCE: ($) => seq(S, O, U, R, C, E),
    SOURCE_COMPUTER: ($) =>
      seq(S, O, U, R, C, E, MINUSCHAR, C, O, M, P, U, T, E, R),
    SPACE: ($) => seq(S, P, A, C, E),
    SPACES: ($) => seq(S, P, A, C, E, S),
    SPECIAL_NAMES: ($) => seq(S, P, E, C, I, A, L, MINUSCHAR, N, A, M, E, S),
    SQL: ($) => seq(S, Q, L),
    STANDARD: ($) => seq(S, T, A, N, D, A, R, D),
    STANDARD_1: ($) => seq(S, T, A, N, D, A, R, D, MINUSCHAR, "1"),
    STANDARD_2: ($) => seq(S, T, A, N, D, A, R, D, MINUSCHAR, "2"),
    START: ($) => seq(S, T, A, R, T),
    STATUS: ($) => seq(S, T, A, T, U, S),
    STOP: ($) => seq(S, T, O, P),
    STRING: ($) => seq(S, T, R, I, N, G),
    SUB_QUEUE_1: ($) => seq(S, U, B, MINUSCHAR, Q, U, E, U, E, MINUSCHAR, "1"),
    SUB_QUEUE_2: ($) => seq(S, U, B, MINUSCHAR, Q, U, E, U, E, MINUSCHAR, "2"),
    SUB_QUEUE_3: ($) => seq(S, U, B, MINUSCHAR, Q, U, E, U, E, MINUSCHAR, "3"),
    SUBTRACT: ($) => seq(S, U, B, T, R, A, C, T),
    SUM: ($) => seq(S, U, M),
    SUPPRESS: ($) => seq(S, U, P, P, R, E, S, S),
    SYMBOL: ($) => seq(S, Y, M, B, O, L),
    SYMBOLIC: ($) => seq(S, Y, M, B, O, L, I, C),
    SYNC: ($) => seq(S, Y, N, C),
    SYNCHRONIZED: ($) => seq(S, Y, N, C, H, R, O, N, I, Z, E, D),
    TABLE: ($) => seq(T, A, B, L, E),
    TALLY: ($) => seq(T, A, L, L, Y),
    TALLYING: ($) => seq(T, A, L, L, Y, I, N, G),
    TASK: ($) => seq(T, A, S, K),
    TAPE: ($) => seq(T, A, P, E),
    TERMINAL: ($) => seq(T, E, R, M, I, N, A, L),
    TERMINATE: ($) => seq(T, E, R, M, I, N, A, T, E),
    TEST: ($) => seq(T, E, S, T),
    TEXT: ($) => seq(T, E, X, T),
    THAN: ($) => seq(T, H, A, N),
    THEN: ($) => seq(T, H, E, N),
    THREAD: ($) => seq(T, H, R, E, A, D),
    THREAD_LOCAL: ($) => seq(T, H, R, E, A, D, MINUSCHAR, L, O, C, A, L),
    THROUGH: ($) => seq(T, H, R, O, U, G, H),
    THRU: ($) => seq(T, H, R, U),
    TIME: ($) => seq(T, I, M, E),
    TIMER: ($) => seq(T, I, M, E, R),
    TIMES: ($) => seq(T, I, M, E, S),
    TITLE: ($) => seq(T, I, T, L, E),
    TO: ($) => seq(T, O),
    TODAYS_DATE: ($) => seq(T, O, D, A, Y, S, MINUSCHAR, D, A, T, E),
    TODAYS_NAME: ($) => seq(T, O, D, A, Y, S, MINUSCHAR, N, A, M, E),
    TOP: ($) => seq(T, O, P),
    TRAILING: ($) => seq(T, R, A, I, L, I, N, G),
    TRUE: ($) => seq(T, R, U, E),
    TRUNCATED: ($) => seq(T, R, U, N, C, A, T, E, D),
    TYPE: ($) => seq(T, Y, P, E),
    TYPEDEF: ($) => seq(T, Y, P, E, D, E, F),
    UNDERLINE: ($) => seq(U, N, D, E, R, L, I, N, E),
    UNIT: ($) => seq(U, N, I, T),
    UNSTRING: ($) => seq(U, N, S, T, R, I, N, G),
    UNTIL: ($) => seq(U, N, T, I, L),
    UP: ($) => seq(U, P),
    UPON: ($) => seq(U, P, O, N),
    USAGE: ($) => seq(U, S, A, G, E),
    USE: ($) => seq(U, S, E),
    USING: ($) => seq(U, S, I, N, G),
    VALUE: ($) => seq(V, A, L, U, E),
    VALUES: ($) => seq(V, A, L, U, E, S),
    VARYING: ($) => seq(V, A, R, Y, I, N, G),
    VIRTUAL: ($) => seq(V, I, R, T, U, A, L),
    WAIT: ($) => seq(W, A, I, T),
    WHEN: ($) => seq(W, H, E, N),
    WHEN_COMPILED: ($) => seq(W, H, E, N, MINUSCHAR, C, O, M, P, I, L, E, D),
    WITH: ($) => seq(W, I, T, H),
    WORDS: ($) => seq(W, O, R, D, S),
    WORKING_STORAGE: ($) =>
      seq(W, O, R, K, I, N, G, MINUSCHAR, S, T, O, R, A, G, E),
    WRITE: ($) => seq(W, R, I, T, E),
    YEAR: ($) => seq(Y, E, A, R),
    YYYYMMDD: ($) => seq(Y, Y, Y, Y, M, M, D, D),
    YYYYDDD: ($) => seq(Y, Y, Y, Y, D, D, D),
    ZERO: ($) => seq(Z, E, R, O),
    ZERO_FILL: ($) => seq(Z, E, R, O, MINUSCHAR, F, I, L, L),
    ZEROS: ($) => seq(Z, E, R, O, S),
    ZEROES: ($) => seq(Z, E, R, O, E, S),
    // symbols
    AMPCHAR: ($) => "&",
    ASTERISKCHAR: ($) => "*",
    DOUBLEASTERISKCHAR: ($) => "**",
    COLONCHAR: ($) => ":",
    COMMACHAR: ($) => ",",
    COMMENTENTRYTAG: ($) => "*>CE",
    COMMENTTAG: ($) => "*>",
    DOLLARCHAR: ($) => "$",
    DOUBLEQUOTE: ($) => '"',
    // period full stop
    DOT_FS: ($) =>
      choice(
        seq(".", repeat1(choice("\r", "\n", "\f", "\t", " "))),
        seq(".", EOF)
      ),
    DOT: ($) => ".",
    EQUALCHAR: ($) => "=",
    EXECCICSTAG: ($) => "*>EXECCICS",
    EXECSQLTAG: ($) => "*>EXECSQL",
    EXECSQLIMSTAG: ($) => "*>EXECSQLIMS",
    LESSTHANCHAR: ($) => "<",
    LESSTHANOREQUAL: ($) => "<=",
    LPARENCHAR: ($) => "(",
    MINUSCHAR: ($) => "-",
    MORETHANCHAR: ($) => ">",
    MORETHANOREQUAL: ($) => ">=",
    NOTEQUALCHAR: ($) => "<>",
    PLUSCHAR: ($) => "+",
    SINGLEQUOTE: ($) => "'",
    RPARENCHAR: ($) => ")",
    SLASHCHAR: ($) => "/",
    // literals
    NONNUMERICLITERAL: ($) =>
      choice(STRINGLITERAL, DBCSLITERAL, HEXNUMBER, NULLTERMINATED),
    _HEXNUMBER: ($) =>
      choice(seq(X, '"', /[0-9A-F]+/, '"'), seq(X, "'", /[0-9A-F]+/, "'")),
    _NULLTERMINATED: ($) =>
      choice(
        seq(Z, '"', repeat(choice(/[^"\n\r]/, '""', "'")), '"'),
        seq(Z, "'", repeat(choice(/[^'\n\r]/, "''", '"')), "'")
      ),
    _STRINGLITERAL: ($) =>
      choice(
        seq('"', repeat(choice(/[^"\n\r]/, '""', "'")), '"'),
        seq("'", repeat(choice(/[^'\n\r]/, "''", '"')), "'")
      ),
    _DBCSLITERAL: ($) =>
      choice(
        seq([GN], '"', repeat(choice(/[^"\n\r]/, '""', "'")), '"'),
        seq([GN], "'", repeat(choice(/[^'\n\r]/, "''", '"')), "'")
      ),
    LEVEL_NUMBER_66: ($) => "66",
    LEVEL_NUMBER_77: ($) => "77",
    LEVEL_NUMBER_88: ($) => "88",
    INTEGERLITERAL: ($) => seq(optional(choice(PLUSCHAR, MINUSCHAR)), /[0-9]+/),
    NUMERICLITERAL: ($) =>
      seq(
        optional(choice(PLUSCHAR, MINUSCHAR)),
        /[0-9]*/,
        choice(DOT, COMMACHAR),
        /[0-9]+/,
        optional(seq(/[eE]/, optional(choice(PLUSCHAR, MINUSCHAR)), /[0-9]+/))
      ),
    IDENTIFIER: ($) => /[a-zA-Z0-9]+([-_]+[a-zA-Z0-9]+)*/,
    // whitespace, line breaks, comments, ...
    NEWLINE: ($) => seq(optional("\r"), "\n"),
    EXECCICSLINE: ($) => seq(EXECCICSTAG, WS, /[^\n\r}]*[\n\r}]/),
    EXECSQLIMSLINE: ($) => seq(EXECSQLIMSTAG, WS, /[^\n\r}]*[\n\r}]/),
    EXECSQLLINE: ($) => seq(EXECSQLTAG, WS, /[^\n\r}]*[\n\r}]/),
    COMMENTENTRYLINE: ($) => seq(COMMENTENTRYTAG, WS, /[^\n\r]*/),
    COMMENTLINE: ($) => seq(COMMENTTAG, WS, /[^\n\r]*/),
    WS: ($) => /[\t\f;]+/,
    SEPARATOR: ($) => ", ",
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
});
