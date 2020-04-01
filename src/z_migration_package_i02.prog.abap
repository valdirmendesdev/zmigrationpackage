*&---------------------------------------------------------------------*
*&  Include           Z_MIGRATION_PACKAGE_I02
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK p1 WITH FRAME TITLE text-001.
PARAMETERS:
p_pkgsrc                          TYPE tadir-devclass OBLIGATORY,
p_pkgdes                          TYPE tadir-devclass OBLIGATORY,
p_reques                          TYPE e070-trkorr    OBLIGATORY,
p_preold                          TYPE fieldname      OBLIGATORY,
p_prenew                          TYPE fieldname      OBLIGATORY,
p_reqsrc                          TYPE e070-trkorr.
SELECTION-SCREEN END OF BLOCK p1.

SELECTION-SCREEN BEGIN OF BLOCK p2 WITH FRAME TITLE text-002.

SELECT-OPTIONS:
s_object                          FOR tadir-object,
s_objs                            FOR tadir-obj_name.

PARAMETERS:
*p_ovrwri                          TYPE c AS CHECKBOX DEFAULT 'X',
p_doma                            TYPE c AS CHECKBOX DEFAULT 'X',
p_dtel                            TYPE c AS CHECKBOX DEFAULT 'X',
p_tabl_s                          TYPE c AS CHECKBOX DEFAULT 'X',
p_tabl                            TYPE c AS CHECKBOX DEFAULT 'X',
p_ttyp                            TYPE c AS CHECKBOX DEFAULT 'X',
p_view                            TYPE c AS CHECKBOX DEFAULT 'X',
p_shlp                            TYPE c AS CHECKBOX DEFAULT 'X',
p_prog                            TYPE c AS CHECKBOX DEFAULT 'X',
p_fugr                            TYPE c AS CHECKBOX DEFAULT 'X',
p_screen                          TYPE c AS CHECKBOX DEFAULT 'X',
p_upview                          TYPE c AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK p2.

SELECTION-SCREEN BEGIN OF BLOCK p3 WITH FRAME TITLE text-003.
PARAMETERS:
p_coptab                          TYPE c AS CHECKBOX DEFAULT ' ',
p_subinc                          TYPE c AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK p3.

SELECTION-SCREEN BEGIN OF BLOCK p4 WITH FRAME TITLE text-004.
PARAMETERS:
p_ssfo                            TYPE c AS CHECKBOX DEFAULT 'X',
p_ssst                            TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK p4.

AT SELECTION-SCREEN ON p_pkgsrc.

AT SELECTION-SCREEN ON p_pkgdes.

AT SELECTION-SCREEN ON p_reques.
