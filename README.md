# tree-sitter-cobol
A tree sitter implementation for COBOL

# Project plan
Cobol is a complicated language to support since it exists in so many variants and dialects. Therefore, this project needs to focus at implementing only partial support and then grow from there.

Short term goals:
 - [ ] parse freeform cobol
 - [ ] parse nicely used copy constructs
 - [ ] parse ans85

task list:
 - [x] Identification Division
 - [x] Environment Division
    - [x] Configuration Section (stub)
    - [x] Input Output Section
        - [x] IO Control Paragraph (almost)
        - [x] File Control Paragraph
            - [x] Record Sequential Files
            - [x] Relative Files
            - [x] Indexed Files
            - [x] Sort Merge Files
 - [ ] Data Division
   - [ ] File Section
   - [ ] Working Storage Section
   - [ ] Linkage Section
   - [ ] Communication Section
   - [ ] Report Section
 - [ ] Procedure Division

# links
 * COBOL Extension for VS Code: https://github.com/spgennard/vscode_cobol
 * Antlr gramma for COBOL: https://github.com/uwol/proleap-cobol-parser
 * Micro Focus Book on their version of COBOL with useful references to ans85, xopen and IBM COBOL, that they support: https://supportline.microfocus.com/documentation/books/oc41books/lrintr.htm (there bight be a newer version of this document)