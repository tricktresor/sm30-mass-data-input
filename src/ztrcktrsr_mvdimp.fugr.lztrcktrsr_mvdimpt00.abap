*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMVDIMP.........................................*
DATA:  BEGIN OF STATUS_ZMVDIMP                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMVDIMP                       .
CONTROLS: TCTRL_ZMVDIMP
            TYPE TABLEVIEW USING SCREEN '0033'.
*.........table declarations:.................................*
TABLES: *ZMVDIMP                       .
TABLES: ZMVDIMP                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
