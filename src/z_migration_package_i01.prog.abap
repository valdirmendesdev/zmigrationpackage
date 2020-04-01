*&---------------------------------------------------------------------*
*&  Include           Z_MIGRATION_PACKAGE_I01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------
* Tables
*----------------------------------------------------------------------
TABLES: tadir.

*----------------------------------------------------------------------
* Constantes
*----------------------------------------------------------------------
CONSTANTS: c_dominio            TYPE tadir-object VALUE 'DOMA',
           c_elemento           TYPE tadir-object VALUE 'DTEL',
           c_tabela             TYPE tadir-object VALUE 'TABL',
           c_categ_tabela       TYPE tadir-object VALUE 'TTYP',
           c_visao              TYPE tadir-object VALUE 'VIEW',
           c_grupo_funcao       TYPE tadir-object VALUE 'FUGR',
           c_ajuda_pesquisa     TYPE tadir-object VALUE 'SHLP',
           c_programa           TYPE tadir-object VALUE 'PROG',
           c_smartforms         TYPE tadir-object VALUE 'SSFO',
           c_smartstyle         TYPE tadir-object VALUE 'SSST'.

CONSTANTS: c_msgid              TYPE sy-msgid     VALUE 'ZVTPR'.

CONSTANTS:
BEGIN OF c_max_length,
  doma                         TYPE i VALUE '30',
  dtel                         TYPE i VALUE '30',
  tabl                         TYPE i VALUE '16',
  tabl_s                       TYPE i VALUE '30',
  ttyp                         TYPE i VALUE '30',
  view                         TYPE i VALUE '16',
  fugr                         TYPE i VALUE '30',
  shlp                         TYPE i VALUE '30',
  prog                         TYPE i VALUE '30',
  ssfo                         TYPE i VALUE '30',
  ssst                         TYPE i VALUE '30',
END OF c_max_length.

*----------------------------------------------------------------------
* Tipos
*----------------------------------------------------------------------
TYPES:
*----------------------------*
* Domínio
*----------------------------*
BEGIN OF y_doma,
  old_name                        TYPE ddobjname,
  new_name                        TYPE ddobjname,
  object                          TYPE tadir-object,
  info                            TYPE dd01v,
END OF y_doma,

yt_doma                           TYPE STANDARD TABLE OF y_doma,

*----------------------------*
* Elemento de Dados
*----------------------------*
BEGIN OF y_dtel,
  old_name                        TYPE ddobjname,
  new_name                        TYPE ddobjname,
  object                          TYPE tadir-object,
  info                            TYPE dd04v,
END OF y_dtel,

yt_dtel                           TYPE STANDARD TABLE OF y_dtel,

*----------------------------*
*   Tabelas e Estruturas
*----------------------------*
BEGIN OF y_tabl,
  old_name                        TYPE ddobjname,
  new_name                        TYPE ddobjname,
  object                          TYPE tadir-object,
  info                            TYPE dd02v,
END OF y_tabl,

yt_tabl                           TYPE STANDARD TABLE OF y_tabl,

*----------------------------------------------------------------------
* Categoria de Tabela
*----------------------------------------------------------------------
BEGIN OF y_ttyp,
  old_name                        TYPE ddobjname,
  new_name                        TYPE ddobjname,
  object                          TYPE tadir-object,
  info                            TYPE dd40v,
END OF y_ttyp,

yt_ttyp                           TYPE STANDARD TABLE OF y_ttyp,

*----------------------------*
* Visão
*----------------------------*
BEGIN OF y_view,
  old_name                        TYPE ddobjname,
  new_name                        TYPE ddobjname,
  object                          TYPE tadir-object,
  info                            TYPE dd25v,
END OF y_view,

yt_view                           TYPE STANDARD TABLE OF y_view,

*----------------------------*
* Grupo de Função
*----------------------------*
BEGIN OF y_fugr,
  old_name                        TYPE rs38l-area,
  new_name                        TYPE rs38l-area,
  object                          TYPE tadir-object,
END OF y_fugr,

yt_fugr                           TYPE STANDARD TABLE OF y_fugr,

*----------------------------*
* Ajuda de pesquisa
*----------------------------*
BEGIN OF y_shlp,
  old_name                        TYPE ddobjname,
  new_name                        TYPE ddobjname,
  object                          TYPE tadir-object,
  info                            TYPE dd30v,
END OF y_shlp,

yt_shlp                           TYPE STANDARD TABLE OF y_shlp,

*----------------------------*
* Programas
*----------------------------*
BEGIN OF y_prog,
  old_name                        TYPE trdir-name,
  new_name                        TYPE trdir-name,
  object                          TYPE tadir-object,
  info                            TYPE trdir,
END OF y_prog,

yt_prog                           TYPE STANDARD TABLE OF y_prog,

*----------------------------*
* Smartforms
*----------------------------*
BEGIN OF y_ssfo,
  old_name                        TYPE trdir-name,
  new_name                        TYPE trdir-name,
  object                          TYPE tadir-object,
END OF y_ssfo,

yt_ssfo                           TYPE STANDARD TABLE OF y_ssfo,

*----------------------------*
* Smartforms
*----------------------------*
BEGIN OF y_ssst,
  old_name                        TYPE trdir-name,
  new_name                        TYPE trdir-name,
  object                          TYPE tadir-object,
END OF y_ssst,

yt_ssst                           TYPE STANDARD TABLE OF y_ssst.


TYPES: yt_zmigration_objs         TYPE STANDARD TABLE OF zmigration_objs.

TYPES:
BEGIN OF y_char_tab,
  line(255)                       TYPE c,
END OF y_char_tab,

yt_char_tab                       TYPE STANDARD TABLE OF y_char_tab,

yt_string                         TYPE STANDARD TABLE OF string,
yt_rssource                       TYPE STANDARD TABLE OF rssource.

*----------------------------------------------------------------------
* Tabelas Globais
*----------------------------------------------------------------------
DATA: gt_bdc                      TYPE tab_bdcdata,
      gt_msg                      TYPE tab_bdcmsgcoll,
      gt_tadir                    TYPE scts_tadir,
      gt_objs                     TYPE yt_zmigration_objs.

*----------------------------------------------------------------------
* Declarações Globais
*----------------------------------------------------------------------
DATA: gw_tadir                    TYPE tadir,
      gw_bdc                      TYPE bdcdata,
      gw_msg                      TYPE bdcmsgcoll,
      gv_modo                     TYPE c, "Modo de execução Batch Input
      gv_existe                   TYPE c,
      go_log                      TYPE REF TO cl_bubas_appl_log_ctx,
      gv_regex_find               TYPE string,
      gv_regex_change             TYPE string,
      gv_regex_module             TYPE string,
      gv_regex_chgmod             TYPE string.

*----------------------------------------------------------------------
* Objetos gerados
*----------------------------------------------------------------------
DATA: gt_doma                     TYPE yt_doma,
      gt_dtel                     TYPE yt_dtel,
      gt_tabl                     TYPE yt_tabl,
      gt_ttyp                     TYPE yt_ttyp,
      gt_view                     TYPE yt_view,
      gt_fugr                     TYPE yt_fugr,
      gt_shlp                     TYPE yt_shlp,
      gt_prog                     TYPE yt_prog,
      gt_ssst                     TYPE yt_ssst,
      gt_ssfo                     TYPE yt_ssfo.
