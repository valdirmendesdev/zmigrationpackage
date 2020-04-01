*----------------------------------------------------------------------*
***INCLUDE Z_MIGRATION_PACKAGE_F01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_PROCESSAMENTO_PRINCIPAL
*&---------------------------------------------------------------------*
FORM f_processamento_principal .

  "Expressão regular para encontrar textos onde iniciem em (m, f ou
  "branco), possua o conteúdo de 'p_preold' seguido de qualquer string.
  CONCATENATE
  '([m|f]|\b)('
  p_preold
  ')(\B)'
  INTO gv_regex_find.

  "Regex de substituição, concatena o novo prefixo +
  "o que encontrou antes + o que encontrou depois
  CONCATENATE p_prenew '$1$3' INTO gv_regex_change.

  CONCATENATE '(sapm)(' p_preold ')(\B)'
  INTO gv_regex_module.

  PERFORM f_seleciona_objetos_pacote.

  PERFORM f_selecionar_objetos_alterados.

  PERFORM f_copiar_dominios.

  PERFORM f_copiar_elementos_dados.

  PERFORM f_copiar_estruturas.

  PERFORM f_copiar_tabelas.

  PERFORM f_copiar_categ_tabelas.

  PERFORM f_copiar_visoes.

  PERFORM f_copiar_ajudas_pesquisa.

  PERFORM f_atualizar_referencias.

  PERFORM f_copiar_conteudo_tabelas.

  PERFORM f_copiar_programas_includes.

  PERFORM f_copiar_grupo_funcoes.

  PERFORM f_substituir_includes_funcoes.

  PERFORM f_copiar_telas.

  PERFORM f_gerar_visao_atualizacao.

  PERFORM f_copiar_smartstyles.

  PERFORM f_copiar_smartforms.

  PERFORM f_gravar_alterados.

  PERFORM f_salvar_log_to_db.

  "[REVISAR] Temporário
  MESSAGE 'Processado finalizado.' TYPE 'S'.

ENDFORM.                    " F_PROCESSAMENTO_PRINCIPAL

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_OBJETOS_PACOTE
*&---------------------------------------------------------------------*
FORM f_seleciona_objetos_pacote .

  DATA: lt_e071                   TYPE STANDARD TABLE OF e071 WITH HEADER LINE.

  REFRESH:  lt_e071,
            gt_tadir.

  IF p_reqsrc IS NOT INITIAL.
    SELECT *
      FROM e071
      INTO TABLE lt_e071
      WHERE trkorr = p_reqsrc.

    IF sy-subrc <> 0.
      REFRESH: lt_e071.
    ENDIF.

    SORT lt_e071 BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_e071 COMPARING object obj_name.

    LOOP AT lt_e071.

      CLEAR: gw_tadir.

      gw_tadir-object   = lt_e071-object.
      gw_tadir-obj_name = lt_e071-obj_name.

      APPEND gw_tadir TO gt_tadir.

    ENDLOOP.

  ENDIF.


  IF gt_tadir[] IS NOT INITIAL.
    SELECT *
      FROM tadir
      INTO TABLE gt_tadir
      FOR ALL ENTRIES IN gt_tadir
      WHERE object   = gt_tadir-object
        AND obj_name = gt_tadir-obj_name
        AND devclass = p_pkgsrc . "Para Debug
  ELSE.

    SELECT *
      FROM tadir
      INTO TABLE gt_tadir
      WHERE object   IN s_object[]
        AND obj_name IN s_objs[]
        AND devclass = p_pkgsrc . "Para Debug
  ENDIF.

  IF sy-subrc <> 0.
    REFRESH: gt_tadir.
  ENDIF.

  SORT gt_tadir BY pgmid object obj_name.

ENDFORM.                    " F_SELECIONA_OBJETOS_PACOTE

"-----------------------------------------------------------------------
" Form copia_elemento
"-----------------------------------------------------------------------
FORM copia_dominio USING    p_src
                            p_new
                            p_devclass
                            p_tadir_old TYPE tadir " [INSERT] - MMSL- CH 9192 -  14.08.2017 17:28:46
                  CHANGING  p_dd01v     TYPE dd01v.

  DATA: lv_name TYPE ddobjname.

  lv_name = p_src.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = lv_name
      state         = 'A'
      langu         = sy-langu
    IMPORTING
      dd01v_wa      = p_dd01v
    EXCEPTIONS
      illegal_input = 1.

  IF sy-subrc <> 0.
    CLEAR: p_dd01v.
    EXIT.
  ENDIF.

  lv_name = p_new.

  p_dd01v-domname = lv_name.

  CALL FUNCTION 'DDIF_DOMA_PUT'
    EXPORTING
      name              = p_new
      dd01v_wa          = p_dd01v
    EXCEPTIONS
      doma_not_found    = 1
      name_inconsistent = 2
      doma_inconsistent = 3
      put_failure       = 4
      put_refused       = 5
      OTHERS            = 6.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_elemento p_src
    '' '1'.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*
* > [BEGIN] MMSL - 9192 -  14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM insert_devclass USING p_devclass
                                p_tadir_old
                                lv_name.

  IF sy-subrc NE 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_elemento p_new
          '' '1'.
    EXIT.
  ENDIF.
*----------------------------------------------------------------------*
* < [END] MMSL - 9192 - 14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM f_ddif_doma_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_elemento p_new
    '' '1'.
    EXIT.
  ENDIF.

ENDFORM. " copia_dominio


*&---------------------------------------------------------------------*
*&      Form  F_COPIA_DOMINIO
*&---------------------------------------------------------------------*
FORM f_copia_dominio  USING     p_src
                                p_new
                                p_devclass
                      CHANGING  p_dd01v     TYPE dd01v.

  REFRESH:  gt_bdc,
            gt_msg.

  PERFORM cria_bdc_tab USING:
*      'X' 'SAPMSRD0'       '0102',
*      ' ' 'BDC_CURSOR'         'RSRD1-DDTYPE_VAL',
*      ' ' 'BDC_OKCODE'         '=COPY',
*      ' ' 'RSRD1-DOMA'         'X',
*      ' ' 'RSRD1-DOMA_VAL'     p_src,

      'X' 'SAPLSD_ENTRY'       '1000',
      ' ' 'BDC_CURSOR'         'RSRD1-DOMA_VAL',
      ' ' 'BDC_OKCODE'         '=WB_COPY',
      ' ' 'RSRD1-DOMA'         'X',
      ' ' 'RSRD1-DOMA_VAL'     p_src,

      'X' 'SAPLSDYY'           '0120',
      ' ' 'BDC_CURSOR'         '*RSEDD0-DDOBJNAME',
      ' ' 'BDC_OKCODE'         '=GOON',
      ' ' 'RSEDD0-DDOBJNAME'   p_src,
      ' ' '*RSEDD0-DDOBJNAME'  p_new,

      'X' 'SAPLSTRD'           '0100',
      ' ' 'BDC_CURSOR'         'KO007-L_DEVCLASS',
      ' ' 'BDC_OKCODE'         '=ADD',
      ' ' 'KO007-L_DEVCLASS'   p_devclass,
      ' ' 'KO007-L_AUTHOR'     sy-uname,

      'X' 'SAPLSTRD'           '0300',
      ' ' 'BDC_CURSOR'         'KO008-TRKORR',
      ' ' 'BDC_OKCODE'         '=LOCK',
      ' ' 'KO008-TRKORR'       p_reques.

*  PERFORM f_TR_CHECK_OBJECT_LOCK USING

  PERFORM cria_bdc_tab USING:
*      'X' 'SAPMSRD0'       '0102',
*      ' ' 'BDC_CURSOR'         'RSRD1-DOMA_VAL',
*      ' ' 'BDC_OKCODE'         '=BACK',
*      ' ' 'RSRD1-DOMA'         'X',
*      ' ' 'RSRD1-DOMA_VAL'     p_new.
      'X' 'SAPLSD_ENTRY'       '1000',
      ' ' 'BDC_CURSOR'         'RSRD1-DOMA_VAL',
      ' ' 'BDC_OKCODE'         '=WB_BACK',
      ' ' 'RSRD1-DOMA'         'X',
      ' ' 'RSRD1-DOMA_VAL'     p_new.

  CALL TRANSACTION 'SE11' USING gt_bdc      "Dados SHDB
                           MODE gv_modo     "Modo execução
                         UPDATE 'S'         "Sincrona
                       MESSAGES INTO gt_msg. "Mensagens

  READ TABLE gt_msg TRANSPORTING NO FIELDS WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_dominio p_src
    '' '1'.
    EXIT.
  ENDIF.

  PERFORM f_ddif_doma_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_dominio p_new
    '' '1'.
    EXIT.
  ENDIF.

*  PERFORM f_ddif_doma_get USING p_new CHANGING p_dd01v.

ENDFORM.                    " F_COPIA_DOMINIO

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_DOMINIOS
*&---------------------------------------------------------------------*
FORM f_copiar_dominios .

  DATA: l_obj_src   TYPE string,
        l_obj_new   TYPE string,
        lw_obj      TYPE y_doma,
        lv_pkgobj   TYPE tdevc-devclass,
        lv_obj_name TYPE tadir-obj_name.

  CHECK p_doma IS NOT INITIAL.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS WITH KEY object = c_dominio
  BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_dominio.
      EXIT.
    ENDIF.

    CLEAR: lw_obj, lv_pkgobj, lv_obj_name.

    l_obj_src = gw_tadir-obj_name.

    PERFORM f_novo_nome_obj USING gw_tadir-object l_obj_src CHANGING
    l_obj_new.

    IF l_obj_new IS INITIAL.
      "[REVISAR] Gravar log?
      PERFORM f_add_msg USING c_msgid 'E' '800' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    lw_obj-old_name = l_obj_src.
    lw_obj-new_name = l_obj_new.

    lv_obj_name = lw_obj-new_name.

    PERFORM f_dd_object_exists USING gw_tadir-object lw_obj-new_name

    CHANGING gv_existe.

    IF gv_existe IS NOT INITIAL.

      " Verificar Pacote. Se não tem, coloca no pacote de destino

*      CALL FUNCTION 'DEV_GET_DEVCLASS_FROM_OBJECT'
*        EXPORTING
*          i_pgmid    = gw_tadir-pgmid
*          i_objtype  = c_dominio
*          i_objname  = lv_obj_name
*        IMPORTING
*          e_devclass = lv_pkgobj.
*
*      IF sy-subrc EQ 0 AND lv_pkgobj IS INITIAL.
*
**----------------------------------------------------------------------*
** > [BEGIN] MMSL - 9192 -  14.08.2017 17:30:19
**----------------------------------------------------------------------*
*        PERFORM insert_devclass USING p_pkgdes
*                                      gw_tadir
*                                      lw_obj-new_name.
*
*        IF sy-subrc NE 0.
*          PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_dominio lw_obj-new_name
*                '' '1'.
*          EXIT.
*        ENDIF.
*        CONTINUE.
**----------------------------------------------------------------------*
** < [END] MMSL - 9192 - 14.08.2017 17:30:19
**----------------------------------------------------------------------*
*      ELSE.
*        PERFORM f_add_msg USING c_msgid 'E' '801' gw_tadir-object
*          gw_tadir-obj_name '' '' '1'.
*        CONTINUE.
*      ENDIF.

      PERFORM f_add_msg USING c_msgid 'E' '801' gw_tadir-object
          gw_tadir-obj_name '' '' '1'.
        CONTINUE.


    ENDIF.

    PERFORM copia_dominio USING     lw_obj-old_name
                                    lw_obj-new_name
                                    p_pkgdes
                                    gw_tadir
                          CHANGING  lw_obj-info.

*    PERFORM f_copia_dominio USING     lw_obj-old_name
*                                      lw_obj-new_name
*                                      p_pkgdes
*                            CHANGING  lw_obj-info.

*    IF lw_obj-info IS INITIAL.
*      "[REVISAR] Gravar log?
*      CONTINUE.
*    ENDIF.

    lw_obj-object = gw_tadir-object.
    APPEND lw_obj TO gt_doma.

  ENDLOOP.

  SORT gt_doma BY old_name.

ENDFORM.                    " F_COPIAR_DOMINIOS

*&---------------------------------------------------------------------*
*&      Form  F_NOVO_NOME_OBJ
*&---------------------------------------------------------------------*
FORM f_novo_nome_obj  USING    p_type_obj
                               p_old_name
                      CHANGING p_new_name.

  FIELD-SYMBOLS: <fs_length>      TYPE any.

  DATA: l_new    TYPE string,
        l_length TYPE i,
        l_field  TYPE string.

  CLEAR: p_new_name.

  CONCATENATE 'C_MAX_LENGTH-' p_type_obj INTO l_field.

  ASSIGN (l_field) TO <fs_length>.

  CHECK <fs_length> IS ASSIGNED.

  l_new = p_old_name.

  "[REVISAR] Verificar tipo do objeto?
  REPLACE ALL OCCURRENCES OF    p_preold
                          IN    l_new
                          WITH  p_prenew.

  l_length = strlen( l_new ).

  IF l_length <= <fs_length>.
    p_new_name = l_new.
  ENDIF.

ENDFORM.                    " F_NOVO_NOME_OBJ

*&---------------------------------------------------------------------*
*&      Form  cria_bdc_tab
*&---------------------------------------------------------------------*
FORM cria_bdc_tab USING dynbegin nome valor.

  CLEAR gw_bdc.

  IF dynbegin EQ 'X'.
    MOVE: dynbegin TO gw_bdc-dynbegin,
          nome     TO gw_bdc-program,
          valor    TO gw_bdc-dynpro.
  ELSE.
    MOVE: nome     TO gw_bdc-fnam,
          valor    TO gw_bdc-fval.
  ENDIF.

  APPEND gw_bdc TO gt_bdc.

ENDFORM.      "CRIA_BDC_TAB

*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZATION
*&---------------------------------------------------------------------*
FORM f_initialization .

  gv_modo = 'N'.

  PERFORM f_criar_log.

ENDFORM.                    " F_INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_ELEMENTOS_DADOS
*&---------------------------------------------------------------------*
FORM f_copiar_elementos_dados .

  DATA: l_obj_src   TYPE string,
        l_obj_new   TYPE string,
        lw_obj      TYPE y_dtel,
        lv_pkgobj   TYPE tdevc-devclass,
        lv_obj_name TYPE tadir-obj_name.

  CHECK p_dtel IS NOT INITIAL.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS WITH KEY object =
  c_elemento BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_elemento.
      EXIT.
    ENDIF.

    CLEAR: lw_obj, lv_pkgobj, lv_obj_name.

    l_obj_src = gw_tadir-obj_name.

    lw_obj-old_name = l_obj_src.

    PERFORM f_ddif_dtel_get USING lw_obj-old_name CHANGING lw_obj-info.

    CHECK sy-subrc = 0.

    CHECK lw_obj-info-proxytype IS INITIAL.

    PERFORM f_novo_nome_obj USING gw_tadir-object l_obj_src CHANGING
    l_obj_new.

    IF l_obj_new IS INITIAL.
      "[REVISAR] Gravar log?
      PERFORM f_add_msg USING c_msgid 'E' '800' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    lw_obj-old_name = l_obj_src.
    lw_obj-new_name = l_obj_new.
    lv_obj_name = lw_obj-new_name.

    PERFORM f_dd_object_exists USING gw_tadir-object lw_obj-new_name
    CHANGING gv_existe.

    IF gv_existe IS NOT INITIAL.

      " Verificar Pacote. Se não tem, coloca no pacote de destino

*      CALL FUNCTION 'DEV_GET_DEVCLASS_FROM_OBJECT'
*        EXPORTING
*          i_pgmid    = gw_tadir-pgmid
*          i_objtype  = c_elemento
*          i_objname  = lv_obj_name
*        IMPORTING
*          e_devclass = lv_pkgobj.
*
*      IF sy-subrc EQ 0 AND lv_pkgobj IS INITIAL.
*
**----------------------------------------------------------------------*
** > [BEGIN] MMSL - 9192 -  14.08.2017 17:30:19
**----------------------------------------------------------------------*
*        PERFORM insert_devclass USING p_pkgdes
*                                      gw_tadir
*                                      lw_obj-new_name.
*
*        IF sy-subrc NE 0.
*          PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_elemento lw_obj-new_name
*                '' '1'.
*          EXIT.
*        ENDIF.
*        CONTINUE.
**----------------------------------------------------------------------*
** < [END] MMSL - 9192 - 14.08.2017 17:30:19
**----------------------------------------------------------------------*
*      ELSE.
*        PERFORM f_add_msg USING c_msgid 'E' '801' gw_tadir-object
*          gw_tadir-obj_name '' '' '1'.
*        CONTINUE.
*      ENDIF.

      PERFORM f_add_msg USING c_msgid 'E' '801' gw_tadir-object
          gw_tadir-obj_name '' '' '1'.
        CONTINUE.

    ENDIF.

    PERFORM copia_elemento USING     lw_obj-old_name
                                        lw_obj-new_name
                                        p_pkgdes
                                        gw_tadir
                              CHANGING  lw_obj-info.

*    PERFORM f_copia_elemento  USING     lw_obj-old_name
*                                        lw_obj-new_name
*                                        p_pkgdes
*                              CHANGING  lw_obj-info.

*    IF lw_obj-info IS INITIAL.
*      "[REVISAR] Gravar log?
*      CONTINUE.
*    ENDIF.

    lw_obj-object = gw_tadir-object.

    APPEND lw_obj TO gt_dtel.

  ENDLOOP.

  SORT gt_dtel BY old_name.

ENDFORM.                    " F_COPIAR_ELEMENTOS_DADOS

"-----------------------------------------------------------------------
" Form copia_elemento
"-----------------------------------------------------------------------
FORM copia_elemento USING     p_src
                                p_new
                                p_devclass
                                p_tadir_old TYPE tadir " [INSERT] - MMSL- CH 9192 -  14.08.2017 17:28:46
                      CHANGING  p_dd04v     TYPE dd04v.

  DATA: lv_name TYPE ddobjname.

  lv_name = p_src.

  CALL FUNCTION 'DDIF_DTEL_GET'
    EXPORTING
      name          = lv_name
      state         = 'A'
      langu         = sy-langu
    IMPORTING
      dd04v_wa      = p_dd04v
    EXCEPTIONS
      illegal_input = 1.

  IF sy-subrc <> 0.
    CLEAR: p_dd04v.
    EXIT.
  ENDIF.

  lv_name = p_new.

  p_dd04v-rollname = lv_name.

  CALL FUNCTION 'DDIF_DTEL_PUT'
    EXPORTING
      name              = p_new
      dd04v_wa          = p_dd04v
    EXCEPTIONS
      dtel_not_found    = 1
      name_inconsistent = 2
      dtel_inconsistent = 3
      put_failure       = 4
      put_refused       = 5
      OTHERS            = 6.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_elemento p_src
    '' '1'.
    EXIT.
  ENDIF.


*----------------------------------------------------------------------*
* > [BEGIN] MMSL - 9192 -  14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM insert_devclass USING p_devclass
                                p_tadir_old
                                lv_name.

  IF sy-subrc NE 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_elemento p_new
          '' '1'.
    EXIT.
  ENDIF.
*----------------------------------------------------------------------*
* < [END] MMSL - 9192 - 14.08.2017 17:30:19
*----------------------------------------------------------------------*

  PERFORM f_ddif_dtel_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_elemento p_new
    '' '1'.
    EXIT.
  ENDIF.

ENDFORM. " copia_elemento

*&---------------------------------------------------------------------*
*&      Form  F_COPIA_ELEMENTO
*&---------------------------------------------------------------------*
FORM f_copia_elemento USING     p_src
                                p_new
                                p_devclass
                      CHANGING  p_dd04v     TYPE dd04v.

  DATA: lw_dominio TYPE y_doma,
        l_rc       TYPE sy-subrc.

  REFRESH:  gt_bdc,
            gt_msg.

  PERFORM cria_bdc_tab USING:
*          'X' 'SAPMSRD0'           '0102',
          'X' 'SAPLSD_ENTRY'       '1000',
          ' ' 'BDC_CURSOR'         'RSRD1-DDTYPE',
          ' ' 'BDC_OKCODE'         '=COPY',
          ' ' 'RSRD1-DDTYPE'       'X',
          ' ' 'RSRD1-DDTYPE_VAL'   p_src,

          'X' 'SAPLSDYY'           '0120',
          ' ' 'BDC_CURSOR'         '*RSEDD0-DDOBJNAME',
          ' ' 'BDC_OKCODE'         '=GOON',
          ' ' 'RSEDD0-DDOBJNAME'   p_src,
          ' ' '*RSEDD0-DDOBJNAME'  p_new,

          'X' 'SAPLSTRD'           '0100',
          ' ' 'BDC_CURSOR'         'KO007-L_DEVCLASS',
          ' ' 'BDC_OKCODE'         '=ADD',
          ' ' 'KO007-L_DEVCLASS'   p_devclass,
          ' ' 'KO007-L_AUTHOR'     sy-uname,

          'X' 'SAPLSTRD'           '0300',
          ' ' 'BDC_CURSOR'         'KO008-TRKORR',
          ' ' 'BDC_OKCODE'         '=LOCK',
          ' ' 'KO008-TRKORR'       p_reques.

  PERFORM cria_bdc_tab USING:
          'X' 'SAPMSRD0'           '0102',
          ' ' 'BDC_CURSOR'         'RSRD1-DDTYPE',
          ' ' 'BDC_OKCODE'         '=BACK',
          ' ' 'RSRD1-DDTYPE'       'X',
          ' ' 'RSRD1-DDTYPE_VAL'   p_new.

  CALL TRANSACTION 'SE11' USING gt_bdc      "Dados SHDB
                         MODE gv_modo     "Modo execução
                       UPDATE 'S'         "Sincrona
                     MESSAGES INTO gt_msg. "Mensagens

  READ TABLE gt_msg TRANSPORTING NO FIELDS WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_elemento p_src
    '' '1'.
    EXIT.
  ENDIF.

  PERFORM f_ddif_dtel_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_elemento p_new
    '' '1'.
    EXIT.
  ENDIF.

*  PERFORM f_ddif_dtel_get USING p_new CHANGING p_dd04v.
*
*  PERFORM f_buscar_doma USING p_dd04v-domname CHANGING lw_dominio.
*
*  CHECK sy-subrc = 0.
*
*  p_dd04v-domname = lw_dominio-new_name.
*
*  PERFORM f_ddif_dtel_put USING p_new p_dd04v.
*
*  IF sy-subrc <> 0.
*    CLEAR: p_dd04v.
*    EXIT.
*  ENDIF.
*
*  PERFORM f_ddif_dtel_activate USING p_new.
*
*  IF sy-subrc <> 0.
*PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_elemento p_new ''
*'1'.
*    EXIT.
*  ENDIF.

ENDFORM.                    " F_COPIA_ELEMENTO

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_DOMA_ACTIVATE
*&---------------------------------------------------------------------*
FORM f_ddif_doma_activate  USING    p_obj.

  CALL FUNCTION 'DDIF_DOMA_ACTIVATE'
    EXPORTING
      name        = p_obj
*     AUTH_CHK    = 'X'
*     PRID        = -1
*   IMPORTING
*     rc          = l_rc
    EXCEPTIONS
      not_found   = 1
      put_failure = 2.

ENDFORM.                    " F_DDIF_DOMA_ACTIVATE

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_DOMA_GET
*&---------------------------------------------------------------------*
FORM f_ddif_doma_get  USING    p_obj
                      CHANGING p_dd01v  TYPE dd01v.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = p_obj
      state         = 'A'
    IMPORTING
      dd01v_wa      = p_dd01v
    EXCEPTIONS
      illegal_input = 1.

  IF sy-subrc <> 0.
    CLEAR: p_dd01v.
  ENDIF.

ENDFORM.                    " F_DDIF_DOMA_GET

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_DTEL_ACTIVATE
*&---------------------------------------------------------------------*
FORM f_ddif_dtel_activate  USING    p_obj.

  CALL FUNCTION 'DDIF_DTEL_ACTIVATE'
    EXPORTING
      name        = p_obj
*     AUTH_CHK    = 'X'
*     PRID        = -1
*   IMPORTING
*     rc          = l_rc
    EXCEPTIONS
      not_found   = 1
      put_failure = 2.

ENDFORM.                    " F_DDIF_DTEL_ACTIVATE

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_DTEL_GET
*&---------------------------------------------------------------------*
FORM f_ddif_dtel_get  USING    p_obj
                      CHANGING p_dd04v  TYPE dd04v.

  CALL FUNCTION 'DDIF_DTEL_GET'
    EXPORTING
      name          = p_obj
      state         = 'A'
      langu         = sy-langu
    IMPORTING
      dd04v_wa      = p_dd04v
    EXCEPTIONS
      illegal_input = 1.

  IF sy-subrc <> 0.
    CLEAR: p_dd04v.
  ENDIF.

ENDFORM.                    " F_DDIF_DTEL_GET

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_DTEL_PUT
*&---------------------------------------------------------------------*
FORM f_ddif_dtel_put  USING    p_obj
                               p_dd04v TYPE dd04v.

  CALL FUNCTION 'DDIF_DTEL_PUT'
    EXPORTING
      name              = p_obj
      dd04v_wa          = p_dd04v
    EXCEPTIONS
      dtel_not_found    = 1
      name_inconsistent = 2
      dtel_inconsistent = 3
      put_failure       = 4
      put_refused       = 5.

ENDFORM.                    " F_DDIF_DTEL_PUT

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_ESTRUTURAS
*&---------------------------------------------------------------------*
FORM f_copiar_estruturas .

  DATA: l_obj_src TYPE string,
        l_obj_new TYPE string,
        lw_obj    TYPE y_tabl,
        lt_dd03p  TYPE dd03ttyp,
        lt_dd05m  TYPE dd05mttyp,
        lt_dd08v  TYPE dd08vttyp,
        lt_dd12v  TYPE dd12vtab,
        lt_dd17v  TYPE dd17vtab,
        lt_dd35v  TYPE dd35vttyp,
        lt_dd36m  TYPE dd36mttyp.

  CHECK p_tabl_s IS NOT INITIAL.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS WITH KEY object = c_tabela
  BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_tabela.
      EXIT.
    ENDIF.

    CLEAR: lw_obj.
    REFRESH:  lt_dd03p,
              lt_dd05m,
              lt_dd08v,
              lt_dd12v,
              lt_dd17v,
              lt_dd35v,
              lt_dd36m.

    lw_obj-old_name = gw_tadir-obj_name.

    PERFORM f_ddif_tabl_get USING     lw_obj-old_name
                            CHANGING  lw_obj-info
                                      lt_dd03p
                                      lt_dd05m
                                      lt_dd08v
                                      lt_dd12v
                                      lt_dd17v
                                      lt_dd35v
                                      lt_dd36m.

    IF lw_obj-info-tabclass = 'APPEND'.
      PERFORM f_add_msg USING c_msgid 'E' '00'  lw_obj-object
                                                lw_obj-new_name
                                                'é append.' '' '1'.
      CONTINUE.
    ENDIF.

    CHECK lw_obj-info-proxytype IS INITIAL.

    "Verifica se é estrutura
    CHECK lw_obj-info IS NOT INITIAL AND lw_obj-info-contflag IS INITIAL
    .

    l_obj_src = gw_tadir-obj_name.

    PERFORM f_novo_nome_obj USING 'TABL_S' l_obj_src CHANGING
    l_obj_new.

    IF l_obj_new IS INITIAL.
      "[REVISAR] Gravar log?
      PERFORM f_add_msg USING c_msgid 'E' '800' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    lw_obj-new_name = l_obj_new.

    PERFORM f_dd_object_exists USING gw_tadir-object lw_obj-new_name
    CHANGING gv_existe.

    IF gv_existe IS NOT INITIAL.
      PERFORM f_add_msg USING c_msgid 'E' '801' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

*    PERFORM f_ddif_tabl_get USING     lw_obj-old_name
*                            CHANGING  lw_obj-info
*                                      lt_dd03p
*                                      lt_dd05m
*                                      lt_dd08v
*                                      lt_dd12v
*                                      lt_dd17v
*                                      lt_dd35v
*                                      lt_dd36m.
*
*    IF lw_obj-info-tabclass = 'APPEND'.
*      PERFORM f_add_msg USING c_msgid 'E' '00'  lw_obj-object
*                                                lw_obj-new_name
*                                                'é append.' '' '1'.
*      CONTINUE.
*    ENDIF.
*
*    "Verifica se é estrutura
*   CHECK lw_obj-info IS NOT INITIAL AND lw_obj-info-contflag IS INITIAL
*   .

    PERFORM copia_estrutura  USING    lw_obj-old_name
                                            lw_obj-new_name
                                            p_pkgdes
                                            gw_tadir
                                  CHANGING  lw_obj-info.

*    PERFORM f_copia_estrutura  USING    lw_obj-old_name
*                                        lw_obj-new_name
*                                        p_pkgdes
*                              CHANGING  lw_obj-info.

*    IF lw_obj-info IS INITIAL.
*      "[REVISAR] Gravar log?
*      CONTINUE.
*    ENDIF.

    CLEAR: lw_obj-info.

    lw_obj-object = gw_tadir-object.

    APPEND lw_obj TO gt_tabl.

  ENDLOOP.

  SORT gt_tabl BY old_name.

ENDFORM.                    " F_COPIAR_ESTRUTURAS

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_TABL_GET
*&---------------------------------------------------------------------*
FORM f_ddif_tabl_get  USING     p_obj
                      CHANGING  p_dd02v   TYPE dd02v
                                pt_dd03p  TYPE dd03ttyp
                                pt_dd05m  TYPE dd05mttyp
                                pt_dd08v  TYPE dd08vttyp
                                pt_dd12v  TYPE dd12vtab
                                pt_dd17v  TYPE dd17vtab
                                pt_dd35v  TYPE dd35vttyp
                                pt_dd36m  TYPE dd36mttyp.

  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = p_obj
      state         = 'M'
*     LANGU         = ' '
    IMPORTING
*     GOTSTATE      =
      dd02v_wa      = p_dd02v
*     DD09L_WA      =
    TABLES
      dd03p_tab     = pt_dd03p
      dd05m_tab     = pt_dd05m
      dd08v_tab     = pt_dd08v
      dd12v_tab     = pt_dd12v
      dd17v_tab     = pt_dd17v
      dd35v_tab     = pt_dd35v
      dd36m_tab     = pt_dd36m
    EXCEPTIONS
      illegal_input = 1.

  IF sy-subrc <> 0.
    CLEAR: p_dd02v.
  ENDIF.

ENDFORM.                    " F_DDIF_TABL_GET


"-----------------------------------------------------------------------
" Form copia_estrutura
"-----------------------------------------------------------------------
FORM copia_estrutura USING    p_src
                            p_new
                            p_devclass
                            p_tadir_old TYPE tadir " [INSERT] - MMSL- CH 9192 -  14.08.2017 17:28:46
                  CHANGING  p_dd02v     TYPE dd02v.

  DATA: lv_name  TYPE ddobjname,
        lt_dd03p TYPE dd03ttyp,
        lw_dd03p LIKE LINE OF lt_dd03p,
        lt_dd05m TYPE dd05mttyp,
        lt_dd08v TYPE dd08vttyp,
        lt_dd12v TYPE dd12vtab,
        lt_dd17v TYPE dd17vtab,
        lt_dd35v TYPE dd35vttyp,
        lt_dd36m TYPE dd36mttyp.

  lv_name = p_src.

  PERFORM f_ddif_tabl_get USING   lv_name
                        CHANGING  p_dd02v
                                  lt_dd03p
                                  lt_dd05m
                                  lt_dd08v
                                  lt_dd12v
                                  lt_dd17v
                                  lt_dd35v
                                  lt_dd36m.

  IF sy-subrc <> 0.
    CLEAR: p_dd02v.
    EXIT.
  ENDIF.

  lv_name = p_new.

  p_dd02v-tabname = lv_name.


  LOOP AT lt_dd03p INTO lw_dd03p.
    IF lw_dd03p-tabname EQ p_src.
      lw_dd03p-tabname = lv_name.
      MODIFY lt_dd03p FROM lw_dd03p.
      CHECK sy-subrc EQ 0.
    ENDIF.
  ENDLOOP.

  PERFORM f_ddif_tabl_put USING lv_name
                                p_dd02v
                                lt_dd03p
                                lt_dd05m
                                lt_dd08v
                                lt_dd35v
                                lt_dd36m.



  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_tabela p_src
    '' '1'.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*
* > [BEGIN] MMSL - 9192 -  14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM insert_devclass USING p_devclass
                                p_tadir_old
                                lv_name.

  IF sy-subrc NE 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_tabela p_new
          '' '1'.
    EXIT.
  ENDIF.
*----------------------------------------------------------------------*
* < [END] MMSL - 9192 - 14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM f_ddif_doma_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_tabela p_new
    '' '1'.
    EXIT.
  ENDIF.

ENDFORM. " copia_estrutura

*&---------------------------------------------------------------------*
*&      Form  F_COPIA_ESTRUTURA
*&---------------------------------------------------------------------*
FORM f_copia_estrutura  USING     p_src
                                  p_new
                                  p_devclass
                        CHANGING  p_dd02v     TYPE dd02v.

  DATA: lt_dd03p   TYPE dd03ttyp,
        lw_dd03p   TYPE dd03p,
        lt_dd05m   TYPE dd05mttyp,
        lt_dd08v   TYPE dd08vttyp,
        lt_dd12v   TYPE dd17v_tab,
        lt_dd17v   TYPE dd17vtab,
        lt_dd35v   TYPE dd35vttyp,
        lt_dd36m   TYPE dd36mttyp,
        lw_dtel    TYPE y_dtel,
        lw_doma    TYPE y_doma,
        l_index    TYPE sy-tabix,
        l_alterado TYPE c.

  REFRESH:  gt_bdc,
            gt_msg.

  PERFORM cria_bdc_tab USING:
          'X' 'SAPMSRD0'           '0102',
          ' ' 'BDC_CURSOR'         'RSRD1-DDTYPE',
          ' ' 'BDC_OKCODE'         '=COPY',
          ' ' 'RSRD1-DDTYPE'       'X',
          ' ' 'RSRD1-DDTYPE_VAL'   p_src,

          'X' 'SAPLSDYY'           '0120',
          ' ' 'BDC_CURSOR'         '*RSEDD0-DDOBJNAME',
          ' ' 'BDC_OKCODE'         '=GOON',
          ' ' 'RSEDD0-DDOBJNAME'   p_src,
          ' ' '*RSEDD0-DDOBJNAME'  p_new,

          'X' 'SAPLSTRD'           '0100',
          ' ' 'BDC_CURSOR'         'KO007-L_DEVCLASS',
          ' ' 'BDC_OKCODE'         '=ADD',
          ' ' 'KO007-L_DEVCLASS'   p_devclass,
          ' ' 'KO007-L_AUTHOR'     sy-uname,

          'X' 'SAPLSTRD'           '0300',
          ' ' 'BDC_CURSOR'         'KO008-TRKORR',
          ' ' 'BDC_OKCODE'         '=LOCK',
          ' ' 'KO008-TRKORR'       p_reques.

  PERFORM cria_bdc_tab USING:
          'X' 'SAPMSRD0'           '0102',
          ' ' 'BDC_CURSOR'         'RSRD1-DDTYPE',
          ' ' 'BDC_OKCODE'         '=BACK',
          ' ' 'RSRD1-DDTYPE'       'X',
          ' ' 'RSRD1-DDTYPE_VAL'   p_new.

  CALL TRANSACTION 'SE11' USING gt_bdc      "Dados SHDB
                       MODE gv_modo     "Modo execução
                     UPDATE 'S'         "Sincrona
                   MESSAGES INTO gt_msg. "Mensagens

  READ TABLE gt_msg TRANSPORTING NO FIELDS WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_tabela p_src ''
    '1'.
    EXIT.
  ENDIF.

  PERFORM f_ddif_tabl_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_tabela p_new ''
    '1'.
    EXIT.
  ENDIF.

*  PERFORM f_ddif_tabl_get USING     p_new
*                          CHANGING  p_dd02v
*                                    lt_dd03p
*                                    lt_dd05m
*                                    lt_dd08v
*                                    lt_dd12v
*                                    lt_dd17v
*                                    lt_dd35v
*                                    lt_dd36m.

*  CHECK p_dd02v IS NOT INITIAL.
*
*  CLEAR: l_alterado.
*
*  "Atualizar elementos de dados e domínios
*  LOOP AT lt_dd03p INTO lw_dd03p.
*
*    l_index = sy-tabix.
*
*    CLEAR:  lw_dtel,
*            lw_doma.
*
*    "Checa elemento de dados
*    PERFORM f_buscar_dtel USING lw_dd03p-rollname CHANGING lw_dtel.
*
*    IF sy-subrc = 0.
*      lw_dd03p-rollname = lw_dtel-new_name.
*      l_alterado = 'X'.
*    ENDIF.
*
*    "Checa domínio
*    PERFORM f_buscar_doma USING lw_dd03p-domname CHANGING lw_doma.
**    READ TABLE gt_dominios INTO lw_doma WITH KEY old_name =
*lw_dd03p-domname BINARY SEARCH.
*
*    IF sy-subrc = 0.
*      lw_dd03p-domname = lw_doma-new_name.
*      l_alterado = 'X'.
*    ENDIF.
*
*    MODIFY lt_dd03p FROM lw_dd03p INDEX l_index.
*
*  ENDLOOP.
*
*  IF l_alterado IS NOT INITIAL.
*    PERFORM f_ddif_tabl_put USING p_new lt_dd03p.
*
*    CHECK sy-subrc = 0.
*
*    PERFORM f_ddif_tabl_activate USING p_new.
*
*    IF sy-subrc <> 0.
*PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_tabela p_new ''
*'1'.
*    ENDIF.
*
*  ENDIF.
*
*  IF sy-subrc <> 0.
*    CLEAR: p_dd02v.
*  ENDIF.

ENDFORM.                    " F_COPIA_ESTRUTURA

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_TABL_ACTIVATE
*&---------------------------------------------------------------------*
FORM f_ddif_tabl_activate  USING    p_obj.

  CALL FUNCTION 'DDIF_TABL_ACTIVATE'
    EXPORTING
      name        = p_obj
*     AUTH_CHK    = 'X'
*     PRID        = -1
*     EXCOMMIT    = 'X'
*   IMPORTING
*     RC          =
    EXCEPTIONS
      not_found   = 1
      put_failure = 2.

ENDFORM.                    " F_DDIF_TABL_ACTIVATE

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_TABL_PUT
*&---------------------------------------------------------------------*
FORM f_ddif_tabl_put  USING     p_obj
                                pw_dd02v TYPE dd02v
                                pt_dd03p TYPE dd03ttyp
                                pt_dd05m TYPE dd05mttyp
                                pt_dd08v TYPE dd08vttyp
                                pt_dd35v TYPE dd35vttyp
                                pt_dd36m TYPE dd36mttyp.

  CALL FUNCTION 'DDIF_TABL_PUT'
    EXPORTING
      name              = p_obj
      dd02v_wa          = pw_dd02v
*     DD09L_WA          = ' '
    TABLES
      dd03p_tab         = pt_dd03p
      dd05m_tab         = pt_dd05m
      dd08v_tab         = pt_dd08v
      dd35v_tab         = pt_dd35v
      dd36m_tab         = pt_dd36m
    EXCEPTIONS
      tabl_not_found    = 1
      name_inconsistent = 2
      tabl_inconsistent = 3
      put_failure       = 4
      put_refused       = 5.


ENDFORM.                    " F_DDIF_TABL_PUT

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_TABELAS
*&---------------------------------------------------------------------*
FORM f_copiar_tabelas .

  DATA: l_obj_src TYPE string,
        l_obj_new TYPE string,
        lw_obj    TYPE y_tabl,
        lt_dd03p  TYPE dd03ttyp,
        lt_dd05m  TYPE dd05mttyp,
        lt_dd08v  TYPE dd08vttyp,
        lt_dd12v  TYPE dd12vtab,
        lt_dd17v  TYPE dd17vtab,
        lt_dd35v  TYPE dd35vttyp,
        lt_dd36m  TYPE dd36mttyp.

  CHECK p_tabl IS NOT INITIAL.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS WITH KEY object = c_tabela
  BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_tabela.
      EXIT.
    ENDIF.

    CLEAR: lw_obj.
    REFRESH: lt_dd03p,
            lt_dd05m,
            lt_dd08v,
            lt_dd12v,
            lt_dd17v,
            lt_dd35v,
            lt_dd36m.

    lw_obj-old_name = gw_tadir-obj_name.

    PERFORM f_ddif_tabl_get USING     lw_obj-old_name
                        CHANGING  lw_obj-info
                                  lt_dd03p
                                  lt_dd05m
                                  lt_dd08v
                                  lt_dd12v
                                  lt_dd17v
                                  lt_dd35v
                                  lt_dd36m.

    CHECK lw_obj-info-proxytype IS INITIAL.

    "Verifica se é tabela (Se possui classe de entrega)
    CHECK lw_obj-info IS NOT INITIAL AND lw_obj-info-contflag IS NOT
    INITIAL.

    l_obj_src = gw_tadir-obj_name.

    PERFORM f_novo_nome_obj USING gw_tadir-object l_obj_src CHANGING
    l_obj_new.

    IF l_obj_new IS INITIAL.
      "[REVISAR] Gravar log?
      PERFORM f_add_msg USING c_msgid 'E' '800' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    lw_obj-old_name = l_obj_src.
    lw_obj-new_name = l_obj_new.

    PERFORM f_dd_object_exists USING gw_tadir-object lw_obj-new_name
    CHANGING gv_existe.

    IF gv_existe IS NOT INITIAL.
      PERFORM f_add_msg USING c_msgid 'E' '801' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

*    PERFORM f_ddif_tabl_get USING     lw_obj-old_name
*                            CHANGING  lw_obj-info
*                                      lt_dd03p
*                                      lt_dd05m
*                                      lt_dd08v
*                                      lt_dd12v
*                                      lt_dd17v
*                                      lt_dd35v
*                                      lt_dd36m.
*
*    "Verifica se é tabela (Se possui classe de entrega)
*    CHECK lw_obj-info IS NOT INITIAL AND lw_obj-info-contflag IS NOT
*    INITIAL.

    PERFORM copia_tabela    USING     lw_obj-old_name
                                            lw_obj-new_name
                                            p_pkgdes
                                            gw_tadir
                                  CHANGING  lw_obj-info.

*    PERFORM f_copia_tabela    USING     lw_obj-old_name
*                                        lw_obj-new_name
*                                        p_pkgdes
*                              CHANGING  lw_obj-info.

*    IF lw_obj-info IS INITIAL.
*      "[REVISAR] Gravar log?
*      CONTINUE.
*    ENDIF.

    CLEAR: lw_obj-info.

    lw_obj-object = gw_tadir-object.

    APPEND lw_obj TO gt_tabl.

  ENDLOOP.

  SORT gt_tabl BY old_name.

ENDFORM.                    " F_COPIAR_TABELAS

"-----------------------------------------------------------------------
" Form copia_tabela
"-----------------------------------------------------------------------
FORM copia_tabela USING     p_src
                                p_new
                                p_devclass
                                p_tadir_old TYPE tadir " [INSERT] - MMSL- CH 9192 -  14.08.2017 17:28:46
                      CHANGING  p_dd02v     TYPE dd02v.

  DATA: lv_name  TYPE ddobjname,
        lt_dd03p TYPE dd03ttyp,
        lw_dd03p LIKE LINE OF lt_dd03p,
        lt_dd05m TYPE dd05mttyp,
        lw_dd05m LIKE LINE OF lt_dd05m,
        lt_dd08v TYPE dd08vttyp,
        lw_dd08v LIKE LINE OF lt_dd08v,
        lt_dd12v TYPE dd12vtab,
        lt_dd17v TYPE dd17vtab,
        lt_dd35v TYPE dd35vttyp,
        lw_dd35v LIKE LINE OF lt_dd35v,
        lt_dd36m TYPE dd36mttyp,
        lw_dd36m LIKE LINE OF lt_dd36m.

  lv_name = p_src.

  PERFORM f_ddif_tabl_get USING     lv_name
                        CHANGING  p_dd02v
                                  lt_dd03p
                                  lt_dd05m
                                  lt_dd08v
                                  lt_dd12v
                                  lt_dd17v
                                  lt_dd35v
                                  lt_dd36m.



  IF sy-subrc <> 0.
    CLEAR: p_dd02v.
    EXIT.
  ENDIF.

  lv_name = p_new.

  p_dd02v-tabname = lv_name.

  LOOP AT lt_dd03p INTO lw_dd03p.
    IF lw_dd03p-tabname EQ p_src.
      lw_dd03p-tabname = lv_name.
      MODIFY lt_dd03p FROM lw_dd03p.
      CHECK sy-subrc EQ 0.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_dd05m INTO lw_dd05m.
    IF lw_dd05m-tabname EQ p_src.
      lw_dd05m-tabname = lv_name.
      MODIFY lt_dd05m FROM lw_dd05m.
      CHECK sy-subrc EQ 0.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_dd08v INTO lw_dd08v.
    IF lw_dd08v-tabname EQ p_src.
      lw_dd08v-tabname = lv_name.
      MODIFY lt_dd08v FROM lw_dd08v.
      CHECK sy-subrc EQ 0.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_dd35v INTO lw_dd35v.
    IF lw_dd35v-tabname EQ p_src.
      lw_dd35v-tabname = lv_name.
      MODIFY lt_dd35v FROM lw_dd35v.
      CHECK sy-subrc EQ 0.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_dd36m INTO lw_dd36m.
    IF lw_dd36m-tabname EQ p_src.
      lw_dd36m-tabname = lv_name.
      MODIFY lt_dd36m FROM lw_dd36m.
      CHECK sy-subrc EQ 0.
    ENDIF.
  ENDLOOP.



  PERFORM f_ddif_tabl_put USING p_new
                                p_dd02v
                                lt_dd03p
                                lt_dd05m
                                lt_dd08v
                                lt_dd35v
                                lt_dd36m.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_elemento p_src
    '' '1'.
    EXIT.
  ENDIF.


*----------------------------------------------------------------------*
* > [BEGIN] MMSL - 9192 -  14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM insert_devclass USING p_devclass
                                p_tadir_old
                                lv_name.

  IF sy-subrc NE 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_elemento p_new
          '' '1'.
    EXIT.
  ENDIF.
*----------------------------------------------------------------------*
* < [END] MMSL - 9192 - 14.08.2017 17:30:19
*----------------------------------------------------------------------*

  PERFORM f_ddif_tabl_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_elemento p_new
    '' '1'.
    EXIT.
  ENDIF.

ENDFORM. " copia_tabela

*&---------------------------------------------------------------------*
*&      Form  F_COPIA_TABELA
*&---------------------------------------------------------------------*
FORM f_copia_tabela USING     p_src
                              p_new
                              p_devclass
                    CHANGING  p_dd02v     TYPE dd02v.

  DATA: lt_dd03p   TYPE dd03ttyp,
        lw_dd03p   TYPE dd03p,
        lt_dd05m   TYPE dd05mttyp,
        lt_dd08v   TYPE dd08vttyp,
        lt_dd12v   TYPE dd12vtab,
        lt_dd17v   TYPE dd17vtab,
        lt_dd35v   TYPE dd35vttyp,
        lt_dd36m   TYPE dd36mttyp,
        lw_dtel    TYPE y_dtel,
        lw_doma    TYPE y_doma,
        l_index    TYPE sy-tabix,
        l_alterado TYPE c.

  REFRESH:  gt_bdc,
            gt_msg.

  PERFORM cria_bdc_tab USING:
          'X' 'SAPMSRD0'            '0102',
          ' ' 'BDC_CURSOR'          'RSRD1-TBMA_VAL',
          ' ' 'BDC_OKCODE'          '=COPY',
          ' ' 'RSRD1-TBMA'          'X',
          ' ' 'RSRD1-TBMA_VAL'      p_src,

          'X' 'SAPLSDYY'            '0122',
          ' ' 'BDC_CURSOR'          '*RSEDD0-DBOBJ_NAME',
          ' ' 'BDC_OKCODE'          '=GOON',
          ' ' 'RSEDD0-DBOBJ_NAME'   p_src,
          ' ' '*RSEDD0-DBOBJ_NAME'  p_new,

          'X' 'SAPLSTRD'            '0100',
          ' ' 'BDC_CURSOR'          'KO007-L_DEVCLASS',
          ' ' 'BDC_OKCODE'          '=ADD',
          ' ' 'KO007-L_DEVCLASS'    p_devclass,
          ' ' 'KO007-L_AUTHOR'      sy-uname,

          'X' 'SAPLSTRD'            '0300',
          ' ' 'BDC_CURSOR'          'KO008-TRKORR',
          ' ' 'BDC_OKCODE'          '=LOCK',
          ' ' 'KO008-TRKORR'        p_reques.

  PERFORM cria_bdc_tab USING:
          'X' 'SAPMSRD0'            '0102',
          ' ' 'BDC_CURSOR'          'RSRD1-TBMA_VAL',
          ' ' 'BDC_OKCODE'          '=BACK',
          ' ' 'RSRD1-TBMA'          'X',
          ' ' 'RSRD1-TBMA_VAL'      p_new.

  CALL TRANSACTION 'SE11' USING gt_bdc      "Dados SHDB
                       MODE gv_modo     "Modo execução
                     UPDATE 'S'         "Sincrona
                   MESSAGES INTO gt_msg. "Mensagens

  READ TABLE gt_msg TRANSPORTING NO FIELDS WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_tabela p_src ''
    '1'.
    EXIT.
  ENDIF.

  PERFORM f_ddif_tabl_get USING     p_new
                        CHANGING  p_dd02v
                                  lt_dd03p
                                  lt_dd05m
                                  lt_dd08v
                                  lt_dd12v
                                  lt_dd17v
                                  lt_dd35v
                                  lt_dd36m.

  IF lt_dd12v[] IS NOT INITIAL.
    PERFORM f_add_msg USING c_msgid 'E' '00' c_tabela p_new
    'possui índice' '' '1'.
  ENDIF.

  PERFORM f_ddif_tabl_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_tabela p_new ''
    '1'.
    EXIT.
  ENDIF.

*  PERFORM f_ddif_tabl_get USING     p_new
*                          CHANGING  p_dd02v
*                                    lt_dd03p
*                                    lt_dd05m
*                                    lt_dd08v
*                                    lt_dd12v
*                                    lt_dd17v
*                                    lt_dd35v
*                                    lt_dd36m.

*  CHECK p_dd02v IS NOT INITIAL.
*
*  CLEAR: l_alterado.
*
*  "Atualizar elementos de dados e domínios
*  LOOP AT lt_dd03p INTO lw_dd03p.
*
*IF  lw_dd03p-fieldname = '.INCLUDE' OR "Se include ou campo faz parte
*de um include
*        lw_dd03p-adminfield <> '0'.
*      CONTINUE.
*    ENDIF.
*
*    l_index = sy-tabix.
*
*    CLEAR:  lw_dtel,
*            lw_doma.
*
*    "Checa elemento de dados
**    READ TABLE gt_elementos INTO lw_dtel WITH KEY old_name =
*lw_dd03p-rollname BINARY SEARCH.
*    PERFORM f_buscar_dtel USING lw_dd03p-rollname CHANGING lw_dtel.
*
*    IF sy-subrc = 0.
*      lw_dd03p-rollname = lw_dtel-new_name.
*      l_alterado = 'X'.
*    ENDIF.
*
*    "Checa domínio
*    PERFORM f_buscar_doma USING lw_dd03p-domname CHANGING lw_doma.
**    READ TABLE gt_dominios INTO lw_doma WITH KEY old_name =
*lw_dd03p-domname BINARY SEARCH.
*
*    IF sy-subrc = 0.
*      lw_dd03p-domname = lw_doma-new_name.
*      l_alterado = 'X'.
*    ENDIF.
*
*    MODIFY lt_dd03p FROM lw_dd03p INDEX l_index.
*
*  ENDLOOP.
*
*  IF l_alterado IS NOT INITIAL.
*    PERFORM f_ddif_tabl_put USING p_new lt_dd03p.
*
*    CHECK sy-subrc = 0.
*
*    PERFORM f_ddif_tabl_activate USING p_new.
*
*    IF sy-subrc <> 0.
*PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_tabela p_new ''
*'1'.
*    ENDIF.
*
*  ENDIF.
*
*  IF sy-subrc <> 0.
*    CLEAR: p_dd02v.
*    EXIT.
*  ENDIF.

*  "Atualizar tabela de valores dos domínios.
*  SORT gt_doma BY info-entitytab.
*
*READ TABLE gt_doma TRANSPORTING NO FIELDS WITH KEY info-entitytab =
*p_src BINARY SEARCH.
*
*  CHECK sy-subrc = 0.
*
*  LOOP AT gt_doma INTO lw_doma FROM sy-tabix.
*
*    l_index = sy-tabix.
*
*    CHECK lw_doma-info-entitytab = p_src.
*
*    lw_doma-info-entitytab = p_new.
*
*    PERFORM f_ddif_doma_put USING lw_doma-new_name lw_doma-info.
*
*    CHECK sy-subrc = 0. "Conseguiu alterar o domínio.
*
*    PERFORM f_ddif_doma_activate USING lw_doma-new_name.
*
*    CHECK sy-subrc = 0. "Conseguiu ativar o domínio.
*
*    MODIFY gt_doma FROM lw_doma INDEX l_index.
*
*  ENDLOOP.
*
*  SORT gt_doma BY old_name.

ENDFORM.                    " F_COPIA_TABELA

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_CATEG_TABELAS
*&---------------------------------------------------------------------*
FORM f_copiar_categ_tabelas .

  DATA: l_obj_src TYPE string,
        l_obj_new TYPE string,
        lw_obj    TYPE y_ttyp.

  CHECK p_ttyp IS NOT INITIAL.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS WITH KEY object =
  c_categ_tabela BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_categ_tabela.
      EXIT.
    ENDIF.

    CLEAR: lw_obj.

    l_obj_src = gw_tadir-obj_name.

    lw_obj-old_name = l_obj_src.

    PERFORM f_ddif_ttyp_get USING lw_obj-old_name CHANGING lw_obj-info.

    CHECK sy-subrc = 0.

    CHECK lw_obj-info-proxytype IS INITIAL.

    PERFORM f_novo_nome_obj USING gw_tadir-object l_obj_src CHANGING
    l_obj_new.

    IF l_obj_new IS INITIAL.
      "[REVISAR] Gravar log?
      PERFORM f_add_msg USING c_msgid 'E' '800' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    lw_obj-old_name = l_obj_src.
    lw_obj-new_name = l_obj_new.

    PERFORM f_dd_object_exists USING gw_tadir-object lw_obj-new_name
    CHANGING gv_existe.

    IF gv_existe IS NOT INITIAL.
      PERFORM f_add_msg USING c_msgid 'E' '801' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    PERFORM copia_categ_tabela  USING     lw_obj-old_name
                                                lw_obj-new_name
                                                p_pkgdes
                                                gw_tadir
                                      CHANGING  lw_obj-info.

*    PERFORM f_copia_categ_tabela  USING     lw_obj-old_name
*                                            lw_obj-new_name
*                                            p_pkgdes
*                                  CHANGING  lw_obj-info.

*    IF lw_obj-info IS INITIAL.
*      "[REVISAR] Gravar log?
*      CONTINUE.
*    ENDIF.

    lw_obj-object = gw_tadir-object.

    APPEND lw_obj TO gt_ttyp.

  ENDLOOP.

  SORT gt_ttyp BY old_name.

ENDFORM.                    " F_COPIAR_CATEG_TABELAS

*&---------------------------------------------------------------------*
*&      Form  F_DD_OBJECT_EXISTS
*&---------------------------------------------------------------------*
FORM f_dd_object_exists  USING    p_class
                                  p_obj
                         CHANGING p_existe.

  DATA: lv_name                   TYPE e071-obj_name.

  CLEAR: p_existe.

  lv_name = p_obj.

  CALL FUNCTION 'DD_OBJECT_EXISTS'
    EXPORTING
      class         = p_class
      name          = lv_name
*     STATE         = 'M'
    IMPORTING
      exists        = gv_existe
*     PRODUCED      =
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_DD_OBJECT_EXISTS

*&---------------------------------------------------------------------*
*&      Form  COPIA_CATEG_TABELA
*&---------------------------------------------------------------------*
FORM copia_categ_tabela USING       p_src
                                    p_new
                                    p_devclass
                                    p_tadir_old TYPE tadir " [INSERT] - MMSL- CH 9192 -  14.08.2017 17:28:46
                          CHANGING  p_dd40v     TYPE dd40v.

  DATA: lv_name TYPE ddobjname.

  lv_name = p_src.

  CALL FUNCTION 'DDIF_TTYP_GET'
    EXPORTING
      name          = lv_name
      state         = 'A'
      langu         = sy-langu
    IMPORTING
      dd40v_wa      = p_dd40v
    EXCEPTIONS
      illegal_input = 1.

  IF sy-subrc <> 0.
    CLEAR: p_dd40v.
    EXIT.
  ENDIF.

  lv_name = p_new.

  p_dd40v-typename = lv_name.

  CALL FUNCTION 'DDIF_TTYP_PUT'
    EXPORTING
      name              = p_new
      dd40v_wa          = p_dd40v
    EXCEPTIONS
      ttyp_not_found    = 1
      name_inconsistent = 2
      ttyp_inconsistent = 3
      put_failure       = 4
      put_refused       = 5
      OTHERS            = 6.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_categ_tabela p_src
    '' '1'.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*
* > [BEGIN] MMSL - 9192 -  14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM insert_devclass USING p_devclass
                                p_tadir_old
                                lv_name.

  IF sy-subrc NE 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_categ_tabela p_new
          '' '1'.
    EXIT.
  ENDIF.
*----------------------------------------------------------------------*
* < [END] MMSL - 9192 - 14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM f_ddif_ttyp_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_categ_tabela p_new
    '' '1'.
    EXIT.
  ENDIF.

ENDFORM.                    " COPIA_CATEG_TABELA

*&---------------------------------------------------------------------*
*&      Form  F_COPIA_CATEG_TABELA
*&---------------------------------------------------------------------*
FORM f_copia_categ_tabela USING     p_src
                                    p_new
                                    p_devclass
                          CHANGING  p_dd40v     TYPE dd40v.

  DATA: lw_struct                 TYPE y_tabl.

  REFRESH:  gt_bdc,
            gt_msg.

  PERFORM cria_bdc_tab USING:
          'X' 'SAPMSRD0'           '0102',
          ' ' 'BDC_CURSOR'         'RSRD1-DDTYPE',
          ' ' 'BDC_OKCODE'         '=COPY',
          ' ' 'RSRD1-DDTYPE'       'X',
          ' ' 'RSRD1-DDTYPE_VAL'   p_src,

          'X' 'SAPLSDYY'           '0120',
          ' ' 'BDC_CURSOR'         '*RSEDD0-DDOBJNAME',
          ' ' 'BDC_OKCODE'         '=GOON',
          ' ' 'RSEDD0-DDOBJNAME'   p_src,
          ' ' '*RSEDD0-DDOBJNAME'  p_new,

          'X' 'SAPLSTRD'           '0100',
          ' ' 'BDC_CURSOR'         'KO007-L_DEVCLASS',
          ' ' 'BDC_OKCODE'         '=ADD',
          ' ' 'KO007-L_DEVCLASS'   p_devclass,
          ' ' 'KO007-L_AUTHOR'     sy-uname,

          'X' 'SAPLSTRD'           '0300',
          ' ' 'BDC_CURSOR'         'KO008-TRKORR',
          ' ' 'BDC_OKCODE'         '=LOCK',
          ' ' 'KO008-TRKORR'       p_reques.

  PERFORM cria_bdc_tab USING:
          'X' 'SAPMSRD0'           '0102',
          ' ' 'BDC_CURSOR'         'RSRD1-DDTYPE',
          ' ' 'BDC_OKCODE'         '=BACK',
          ' ' 'RSRD1-DDTYPE'       'X',
          ' ' 'RSRD1-DDTYPE_VAL'   p_new.

  CALL TRANSACTION 'SE11' USING gt_bdc          "Dados SHDB
                          MODE  gv_modo         "Modo execução
                          UPDATE 'S'            "Sincrona
                          MESSAGES INTO gt_msg. "Mensagens

  READ TABLE gt_msg TRANSPORTING NO FIELDS WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_categ_tabela
    p_src '' '1'.
    EXIT.
  ENDIF.

  PERFORM f_ddif_ttyp_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_categ_tabela
    p_new '' '1'.
    EXIT.
  ENDIF.

*  PERFORM f_ddif_ttyp_get USING p_new CHANGING p_dd40v.

ENDFORM.                    " F_COPIA_CATEG_TABELA

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_TTYP_ACTIVATE
*&---------------------------------------------------------------------*
FORM f_ddif_ttyp_activate  USING    p_new.

  CALL FUNCTION 'DDIF_TTYP_ACTIVATE'
    EXPORTING
      name        = p_new
*     PRID        = -1
*   IMPORTING
*     RC          =
    EXCEPTIONS
      not_found   = 1
      put_failure = 2.

ENDFORM.                    " F_DDIF_TTYP_ACTIVATE

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_TTYP_GET
*&---------------------------------------------------------------------*
FORM f_ddif_ttyp_get  USING    p_new
                      CHANGING p_dd40v  TYPE dd40v.

  CALL FUNCTION 'DDIF_TTYP_GET'
    EXPORTING
      name          = p_new
      state         = 'A'
*     LANGU         = ' '
    IMPORTING
*     GOTSTATE      =
      dd40v_wa      = p_dd40v
*   TABLES
*     DD42V_TAB     =
    EXCEPTIONS
      illegal_input = 1.

  IF sy-subrc <> 0.
    CLEAR: p_dd40v.
  ENDIF.

ENDFORM.                    " F_DDIF_TTYP_GET

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_TTYP_PUT
*&---------------------------------------------------------------------*
FORM f_ddif_ttyp_put  USING    p_new
                               p_dd40v TYPE dd40v.

  CALL FUNCTION 'DDIF_TTYP_PUT'
    EXPORTING
      name              = p_new
      dd40v_wa          = p_dd40v
*   TABLES
*     DD42V_TAB         =
    EXCEPTIONS
      ttyp_not_found    = 1
      name_inconsistent = 2
      ttyp_inconsistent = 3
      put_failure       = 4
      put_refused       = 5.

ENDFORM.                    " F_DDIF_TTYP_PUT

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_DOMA_PUT
*&---------------------------------------------------------------------*
FORM f_ddif_doma_put  USING    p_obj
                               p_dd01v  TYPE dd01v.

  CALL FUNCTION 'DDIF_DOMA_PUT'
    EXPORTING
      name              = p_obj
      dd01v_wa          = p_dd01v
*   TABLES
*     DD07V_TAB         =
    EXCEPTIONS
      doma_not_found    = 1
      name_inconsistent = 2
      doma_inconsistent = 3
      put_failure       = 4
      put_refused       = 5.

ENDFORM.                    " F_DDIF_DOMA_PUT

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_VISOES
*&---------------------------------------------------------------------*
FORM f_copiar_visoes .

  DATA: l_obj_src TYPE string,
        l_obj_new TYPE string,
        lw_obj    TYPE y_view.

  CHECK p_view IS NOT INITIAL.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS WITH KEY object = c_visao
  BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_visao.
      EXIT.
    ENDIF.

    CLEAR: lw_obj.

    l_obj_src = gw_tadir-obj_name.

    PERFORM f_novo_nome_obj USING gw_tadir-object l_obj_src CHANGING
    l_obj_new.

    IF l_obj_new IS INITIAL.
      "[REVISAR] Gravar log?
      PERFORM f_add_msg USING c_msgid 'E' '800' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    lw_obj-old_name = l_obj_src.
    lw_obj-new_name = l_obj_new.

    PERFORM f_dd_object_exists USING gw_tadir-object lw_obj-new_name
    CHANGING gv_existe.

    IF gv_existe IS NOT INITIAL.
      PERFORM f_add_msg USING c_msgid 'E' '801' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    PERFORM copia_visao USING     lw_obj-old_name
                                   lw_obj-new_name
                                   p_pkgdes
                                   gw_tadir
                         CHANGING  lw_obj-info.

*    PERFORM f_copia_visao USING     lw_obj-old_name
*                                    lw_obj-new_name
*                                    p_pkgdes
*                          CHANGING  lw_obj-info.

*    IF lw_obj-info IS INITIAL.
*      "[REVISAR] Gravar log?
*      CONTINUE.
*    ENDIF.

    lw_obj-object = gw_tadir-object.

    APPEND lw_obj TO gt_view.

  ENDLOOP.

  SORT gt_view BY old_name.

ENDFORM.                    " F_COPIAR_VISOES


"-----------------------------------------------------------------------
" Form copia_visao
"-----------------------------------------------------------------------
FORM copia_visao USING    p_src
                            p_new
                            p_devclass
                            p_tadir_old TYPE tadir " [INSERT] - MMSL- CH 9192 -  14.08.2017 17:28:46
                  CHANGING  p_dd25v     TYPE dd25v.

  DATA: lv_name  TYPE ddobjname,
        lt_dd26v TYPE STANDARD TABLE OF dd26v,
        lw_dd26v LIKE LINE OF lt_dd26v,
        lt_dd27p TYPE STANDARD TABLE OF dd27p,
        lw_dd27p LIKE LINE OF lt_dd27p,
        lt_dd28j TYPE STANDARD TABLE OF dd28j,
        lw_dd28j LIKE LINE OF lt_dd28j,
        lt_dd28v TYPE STANDARD TABLE OF dd28v,
        lw_dd28v LIKE LINE OF lt_dd28v.

  lv_name = p_src.

  CALL FUNCTION 'DDIF_VIEW_GET'
    EXPORTING
      name          = lv_name
      state         = 'A'
      langu         = sy-langu
    IMPORTING
*     GOTSTATE      =
      dd25v_wa      = p_dd25v
*     DD09L_WA      =
    TABLES
      dd26v_tab     = lt_dd26v
      dd27p_tab     = lt_dd27p
      dd28j_tab     = lt_dd28j
      dd28v_tab     = lt_dd28v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.


  IF sy-subrc <> 0.
    CLEAR: p_dd25v.
    EXIT.
  ENDIF.

  lv_name = p_new.

  p_dd25v-viewname = lv_name.


  LOOP AT lt_dd26v INTO lw_dd26v.
    IF lw_dd26v-viewname EQ p_src.
      lw_dd26v-viewname = lv_name.
      MODIFY lt_dd26v FROM lw_dd26v.
      CHECK sy-subrc EQ 0.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_dd27p INTO lw_dd27p.
    IF lw_dd27p-viewname EQ p_src.
      lw_dd27p-viewname = lv_name.
      MODIFY lt_dd27p FROM lw_dd27p.
      CHECK sy-subrc EQ 0.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_dd28j INTO lw_dd28j.
    IF lw_dd28j-viewname EQ p_src.
      lw_dd28j-viewname = lv_name.
      MODIFY lt_dd28j FROM lw_dd28j.
      CHECK sy-subrc EQ 0.
    ENDIF.
  ENDLOOP.

*  LOOP AT lt_dd28v INTO lw_dd28v.
*    IF lw_dd28v-viewname EQ p_src.
*      lw_dd28v-viewname = lv_name.
*      MODIFY lt_dd28v FROM lw_dd28v.
*      CHECK sy-subrc EQ 0.
*    ENDIF.
*  ENDLOOP.

  CALL FUNCTION 'DDIF_VIEW_PUT'
    EXPORTING
      name              = p_new
      dd25v_wa          = p_dd25v
*     DD09L_WA          = ' '
    TABLES
      dd26v_tab         = lt_dd26v
      dd27p_tab         = lt_dd27p
      dd28j_tab         = lt_dd28j
      dd28v_tab         = lt_dd28v
    EXCEPTIONS
      view_not_found    = 1
      name_inconsistent = 2
      view_inconsistent = 3
      put_failure       = 4
      put_refused       = 5
      OTHERS            = 6.




  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_visao p_src
    '' '1'.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*
* > [BEGIN] MMSL - 9192 -  14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM insert_devclass USING p_devclass
                                p_tadir_old
                                lv_name.

  IF sy-subrc NE 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_visao p_new
          '' '1'.
    EXIT.
  ENDIF.
*----------------------------------------------------------------------*
* < [END] MMSL - 9192 - 14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM f_ddif_view_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_visao p_new
    '' '1'.
    EXIT.
  ENDIF.

ENDFORM. " copia_visao

*&---------------------------------------------------------------------*
*&      Form  F_COPIA_VISAO
*&---------------------------------------------------------------------*
FORM f_copia_visao  USING     p_src
                              p_new
                              p_devclass
                    CHANGING  p_dd25v  TYPE dd25v.

  REFRESH:  gt_bdc,
            gt_msg.

  PERFORM cria_bdc_tab USING:
          'X' 'SAPMSRD0'           '0102',
          ' ' 'BDC_CURSOR'         'RSRD1-VIMA_VAL',
          ' ' 'BDC_OKCODE'         '=COPY',
          ' ' 'RSRD1-VIMA'         'X',
          ' ' 'RSRD1-VIMA_VAL'     p_src,

          'X' 'SAPLSDYY'           '0122',
          ' ' 'BDC_CURSOR'         '*RSEDD0-DBOBJ_NAME',
          ' ' 'BDC_OKCODE'         '=GOON',
          ' ' 'RSEDD0-DBOBJ_NAME'  p_src,
          ' ' '*RSEDD0-DBOBJ_NAME' p_new,

          'X' 'SAPLSTRD'           '0100',
          ' ' 'BDC_CURSOR'         'KO007-L_DEVCLASS',
          ' ' 'BDC_OKCODE'         '=ADD',
          ' ' 'KO007-L_DEVCLASS'   p_devclass,
          ' ' 'KO007-L_AUTHOR'     sy-uname,

          'X' 'SAPLSTRD'           '0300',
          ' ' 'BDC_CURSOR'         'KO008-TRKORR',
          ' ' 'BDC_OKCODE'         '=LOCK',
          ' ' 'KO008-TRKORR'       p_reques.

  PERFORM cria_bdc_tab USING:
          'X' 'SAPMSRD0'           '0102',
          ' ' 'BDC_CURSOR'         'RSRD1-VIMA_VAL',
          ' ' 'BDC_OKCODE'         '=BACK',
          ' ' 'RSRD1-VIMA'         'X',
          ' ' 'RSRD1-VIMA_VAL'     p_new.

  CALL TRANSACTION 'SE11' USING gt_bdc          "Dados SHDB
                          MODE  gv_modo         "Modo execução
                          UPDATE 'S'            "Sincrona
                          MESSAGES INTO gt_msg. "Mensagens

  READ TABLE gt_msg TRANSPORTING NO FIELDS WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_visao p_src ''
    '1'.
    EXIT.
  ENDIF.

  PERFORM f_ddif_view_activate USING p_new.

  IF sy-subrc = 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_visao p_new ''
    '1'.
    EXIT.
  ENDIF.

  "[REVISAR] Acho mais prudente alterar as visões manualmente

ENDFORM.                    " F_COPIA_VISAO

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_VIEW_ACTIVATE
*&---------------------------------------------------------------------*
FORM f_ddif_view_activate  USING p_new.

  CALL FUNCTION 'DDIF_VIEW_ACTIVATE'
    EXPORTING
      name        = p_new
*     AUTH_CHK    = 'X'
*     PRID        = -1
*   IMPORTING
*     RC          =
    EXCEPTIONS
      not_found   = 1
      put_failure = 2.

ENDFORM.                    " F_DDIF_VIEW_ACTIVATE

*&---------------------------------------------------------------------*
*&      Form  F_ADD_MSG
*&---------------------------------------------------------------------*
FORM f_add_msg  USING    p_msgid
                         p_msgty
                         p_msgno
                         p_msgv1
                         p_msgv2
                         p_msgv3
                         p_msgv4
                         p_probclass.

  DATA: l_msg                     TYPE bubas_s_msg.

  CHECK go_log IS NOT INITIAL.

  CLEAR: l_msg.

  l_msg-msgid     = p_msgid.
  l_msg-msgty     = p_msgty.
  l_msg-msgno     = p_msgno.
  l_msg-msgv1     = p_msgv1.
  l_msg-msgv2     = p_msgv2.
  l_msg-msgv3     = p_msgv3.
  l_msg-msgv4     = p_msgv4.
  l_msg-probclass = p_probclass.

  CALL METHOD go_log->add_message
    EXPORTING
      i_s_msg          = l_msg
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_ADD_MSG

*&---------------------------------------------------------------------*
*&      Form  F_SALVAR_LOG_TO_DB
*&---------------------------------------------------------------------*
FORM f_salvar_log_to_db .

  CHECK go_log IS NOT INITIAL.

  CALL METHOD go_log->save_to_db
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      OTHERS           = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_SALVAR_LOG_TO_DB

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_GRUPO_FUNCOES
*&---------------------------------------------------------------------*
FORM f_copiar_grupo_funcoes .

  DATA: l_obj_src    TYPE string,
        l_obj_new    TYPE string,
        lw_obj       TYPE y_fugr,
        l_sm30       TYPE c,
        l_regex_find TYPE string.

  CHECK p_fugr IS NOT INITIAL.

  CONCATENATE '(' p_prenew ')(.*)' INTO l_regex_find.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS WITH KEY object =
  c_grupo_funcao BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_grupo_funcao.
      EXIT.
    ENDIF.

    l_obj_src = gw_tadir-obj_name.

    PERFORM f_novo_nome_fugr USING gw_tadir-object l_obj_src ''
           CHANGING l_obj_new.

    IF l_obj_new IS INITIAL.
      "[REVISAR] Gravar log?
      PERFORM f_add_msg USING c_msgid 'E' '800' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    lw_obj-old_name = l_obj_src.
    lw_obj-new_name = l_obj_new.

    PERFORM f_function_pool_exists USING gw_tadir-object lw_obj-new_name
    CHANGING gv_existe.

    IF gv_existe IS NOT INITIAL.
      PERFORM f_add_msg USING c_msgid 'W' '801' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.

      PERFORM f_grupo_funcao_sm30 USING lw_obj-old_name CHANGING l_sm30.

      IF l_sm30 IS NOT INITIAL.
        CONTINUE.
      ENDIF.

    ELSE.

      PERFORM f_grupo_funcao_sm30 USING lw_obj-old_name CHANGING l_sm30.

      IF l_sm30 IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      PERFORM f_copia_grupo_funcao USING lw_obj-old_name lw_obj-new_name
            .
    ENDIF.

    CHECK sy-subrc = 0.

    lw_obj-object = gw_tadir-object.

    APPEND lw_obj TO gt_fugr.

    PERFORM f_copia_funcoes USING lw_obj-old_name lw_obj-new_name
    CHANGING gt_fugr.

  ENDLOOP.

  SORT gt_fugr BY old_name.

ENDFORM.                    " F_COPIAR_GRUPO_FUNCOES

*&---------------------------------------------------------------------*
*&      Form  F_NOVO_NOME_FUGR
*&---------------------------------------------------------------------*
FORM f_novo_nome_fugr USING     p_type_obj
                                p_old_name
                                p_function_module
                      CHANGING  p_new_name.

  DATA: c_max_length    TYPE i VALUE 30,
        l_new           TYPE string,
        l_length        TYPE i,
        l_fugr_name     TYPE rs38l-area,
        l_function_name TYPE rs38l-name.

  CLEAR: p_new_name.

  IF p_function_module IS NOT INITIAL.
    DESCRIBE FIELD l_function_name LENGTH c_max_length IN CHARACTER MODE
    .
  ELSE.
    DESCRIBE FIELD l_fugr_name LENGTH c_max_length IN CHARACTER MODE.
  ENDIF.

  l_new = p_old_name.

  "Se é módulo de função, não colocar o namespace antes
  IF p_function_module IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF    p_preold
                            IN    l_new
                            WITH  ''.
  ELSE.
    REPLACE ALL OCCURRENCES OF    p_preold
                            IN    l_new
                            WITH  p_prenew.
  ENDIF.

  l_length = strlen( l_new ).

  IF l_length <= c_max_length.
    p_new_name = l_new.
  ENDIF.

ENDFORM.                    " F_NOVO_NOME_FUGR

*&---------------------------------------------------------------------*
*&      Form  F_FUNCTION_POOL_EXISTS
*&---------------------------------------------------------------------*
FORM f_function_pool_exists  USING    p_class
                                      p_obj
                             CHANGING p_existe.

  DATA: l_function                TYPE tlibg-area.

  p_existe = 'X'.

  l_function = p_obj.

  CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
    EXPORTING
      function_pool   = l_function
    EXCEPTIONS
      pool_not_exists = 1.

  IF sy-subrc <> 0.
    CLEAR: p_existe.
  ENDIF.

ENDFORM.                    " F_FUNCTION_POOL_EXISTS

*&---------------------------------------------------------------------*
*&      Form  F_COPIA_GRUPO_FUNCAO
*&---------------------------------------------------------------------*
FORM f_copia_grupo_funcao  USING  p_src
                                  p_new.

  CALL FUNCTION 'RS_FUNCTION_POOL_COPY'
    EXPORTING
      function_pool          = p_src
      function_pool_copy     = p_new
      korrnum                = p_reques
      without_functions      = 'X'
*     SHORT_TEXT             =
*     RESPONSIBLE            =
*     WB_FB_MANAGER          =
      devclass               = p_pkgdes
*   IMPORTING
*     CORRNUM                =
*     NEW_FUNCTION_POOL      =
    EXCEPTIONS
      function_pool_exist    = 1
      cancelled_in_corr      = 2
      enqueue_system_failure = 3
      in_progress            = 4
      pool_not_exist         = 5
      cancelled              = 6.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_grupo_funcao
    p_src '' '1'.
    EXIT.
  ENDIF.

ENDFORM.                    " F_COPIA_GRUPO_FUNCAO

*&---------------------------------------------------------------------*
*&      Form  F_COPIA_FUNCOES
*&---------------------------------------------------------------------*
FORM f_copia_funcoes  USING     p_src
                                p_new
                      CHANGING  pt_objs TYPE   yt_fugr.

  DATA: lw_obj      TYPE y_fugr,
        lt_code1    TYPE yt_char_tab,
        lt_rssource TYPE yt_rssource,
        l_tabix     TYPE sy-tabix,
        l_grupo     TYPE rs38l-area,
        l_namespace TYPE rs38l-namespace,
        lw_tftit    TYPE tftit.

  DATA: lt_funcoes            TYPE STANDARD TABLE OF rs38l_incl,
        lw_funcao             TYPE rs38l_incl,
        lw_nova_funcao        TYPE rs38l_incl,
        l_global              TYPE rs38l-global,
        l_remote              TYPE rs38l-remote,
        l_utask               TYPE rs38l-utask,
        l_exception_classes   TYPE enlfdir-exten3,
        lt_exception_list     TYPE STANDARD TABLE OF rsexc,
        lw_exception_list     TYPE rsexc,
        lt_export_parameter   TYPE STANDARD TABLE OF rsexp,
        lw_export_parameter   TYPE rsexp,
        lt_import_parameter   TYPE STANDARD TABLE OF rsimp,
        lw_import_parameter   TYPE rsimp,
        lt_changing_parameter TYPE STANDARD TABLE OF rscha,
        lw_changing_parameter TYPE rscha,
        lt_tables_parameter   TYPE STANDARD TABLE OF rstbl,
        lw_tables_parameter   TYPE rstbl,
        lt_p_docu             TYPE STANDARD TABLE OF rsfdo,
        lt_enha_exp_parameter TYPE STANDARD TABLE OF rsexp,
        lt_enha_imp_parameter TYPE STANDARD TABLE OF rsimp,
        lt_enha_cha_parameter TYPE STANDARD TABLE OF rscha,
        lt_enha_tbl_parameter TYPE STANDARD TABLE OF rstbl,
        lt_enha_docu          TYPE STANDARD TABLE OF rsfdo,
        lw_include_name       TYPE rs38l-include.

  "Busca todas as funções que o grupo de funções possui
  CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
    EXPORTING
      function_pool           = p_src
    TABLES
      functab                 = lt_funcoes
    EXCEPTIONS
      function_pool_not_found = 1.

  CHECK sy-subrc = 0.

  l_namespace = p_prenew.

  l_grupo = p_new.

  "[REVISAR] Verificar tipo do objeto?
  REPLACE ALL OCCURRENCES OF    l_namespace
                          IN    l_grupo
                          WITH  ''.

  CONDENSE l_grupo NO-GAPS.

  LOOP AT lt_funcoes INTO lw_funcao.

    CLEAR:  lw_nova_funcao,
            lw_obj.

    "Não executar copia de função sm30
    IF lw_funcao-funcname CS 'TABLEFRAME' OR
       lw_funcao-funcname CS 'TABLEPROC_' OR
       lw_funcao-funcname CS 'VIEWFRAME' OR
       lw_funcao-funcname CS 'VIEWPROC_'.
      CONTINUE.

    ENDIF.

    "Carrega dados e parâmetros da função
    CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
      EXPORTING
        funcname           = lw_funcao-funcname
      IMPORTING
        global_flag        = l_global
        remote_call        = l_remote
        update_task        = l_utask
        exception_classes  = l_exception_classes
      TABLES
        exception_list     = lt_exception_list
        export_parameter   = lt_export_parameter
        import_parameter   = lt_import_parameter
        changing_parameter = lt_changing_parameter
        tables_parameter   = lt_tables_parameter
        p_docu             = lt_p_docu
        enha_exp_parameter = lt_enha_exp_parameter
        enha_imp_parameter = lt_enha_imp_parameter
        enha_cha_parameter = lt_enha_cha_parameter
        enha_tbl_parameter = lt_enha_tbl_parameter
        enha_docu          = lt_enha_docu
      EXCEPTIONS
        error_message      = 1
        function_not_found = 2
        invalid_name       = 3.

    "Gera o novo nome da função
    PERFORM f_novo_nome_fugr USING c_grupo_funcao lw_funcao-funcname ''
    CHANGING lw_nova_funcao-funcname.

    IF lw_nova_funcao-funcname IS INITIAL.
      PERFORM f_add_msg USING c_msgid 'E' '800' c_grupo_funcao p_src
      lw_funcao-funcname '' '1'.
      CONTINUE.
    ENDIF.

    lw_obj-old_name = lw_funcao-funcname.
    lw_obj-new_name = lw_nova_funcao-funcname.
    lw_obj-object   = c_grupo_funcao.

    "Normalizar o código do include para 72 posições por linha
    PERFORM f_rs_source_split_to_72 USING lw_funcao-include.

    CHECK sy-subrc = 0.

    "Lê o conteúdo do include da função
    READ REPORT lw_funcao-include INTO lt_rssource.

    CHECK sy-subrc = 0.

    PERFORM f_remover_linhas_include USING 'X' CHANGING lt_rssource.

    "Atualiza parâmetros de importação
    LOOP AT lt_import_parameter INTO lw_import_parameter.
      l_tabix = sy-tabix.

      PERFORM f_busca_tipo_parameter CHANGING lw_import_parameter-typ.

      MODIFY lt_import_parameter FROM lw_import_parameter INDEX l_tabix.

    ENDLOOP.

    "Atualiza parâmetros de exportação
    LOOP AT lt_export_parameter INTO lw_export_parameter.
      l_tabix = sy-tabix.

      PERFORM f_busca_tipo_parameter CHANGING lw_export_parameter-typ.

      MODIFY lt_export_parameter FROM lw_export_parameter INDEX l_tabix.

    ENDLOOP.

    "Atualiza parâmetros changing
    LOOP AT lt_changing_parameter INTO lw_changing_parameter.
      l_tabix = sy-tabix.

      PERFORM f_busca_tipo_parameter CHANGING lw_changing_parameter-typ.

      MODIFY lt_changing_parameter FROM lw_changing_parameter INDEX
      l_tabix.

    ENDLOOP.

    "Atualiza parâmetros tables
    LOOP AT lt_tables_parameter INTO lw_tables_parameter.
      l_tabix = sy-tabix.

      PERFORM f_busca_tipo_parameter CHANGING lw_tables_parameter-typ.

      MODIFY lt_tables_parameter FROM lw_tables_parameter INDEX l_tabix.

    ENDLOOP.

    SELECT SINGLE *
      FROM tftit
      INTO lw_tftit
      WHERE spras = sy-langu
        AND funcname = lw_funcao-funcname.

    CHECK sy-subrc = 0.

*    CLEAR: lw_include_proc.

    CALL FUNCTION 'RS_FUNCTIONMODULE_INSERT'
      EXPORTING
        funcname                = lw_nova_funcao-funcname
        function_pool           = l_grupo
        short_text              = lw_tftit-stext
        update_task             = 'X'
        corrnum                 = p_reques
        namespace               = l_namespace  "/VTAX/
      IMPORTING
        function_include        = lw_include_name
      TABLES
        import_parameter        = lt_import_parameter
        export_parameter        = lt_export_parameter
        tables_parameter        = lt_tables_parameter
        changing_parameter      = lt_changing_parameter
        exception_list          = lt_exception_list
        parameter_docu          = lt_p_docu
        source                  = lt_rssource
      EXCEPTIONS
        double_task             = 1
        error_message           = 2
        function_already_exists = 3
        invalid_function_pool   = 4
        invalid_name            = 5
        too_many_functions      = 6
        no_modify_permission    = 7
        no_show_permission      = 8
        enqueue_system_failure  = 9
        canceled_in_corr        = 10.


    IF sy-subrc <> 0.
      PERFORM f_add_msg USING c_msgid 'E' '802' text-t01 c_grupo_funcao
      p_new lw_nova_funcao-funcname '1'.
      CONTINUE.
    ENDIF.

    APPEND lw_obj TO pt_objs.
*
*    lw_include_proc-include = lw_include_name.
*
*    APPEND lw_include_proc TO lt_includes_proc.

  ENDLOOP.

**----------------------------*
** Atualizar Includes
**----------------------------*
*
*  SORT lt_includes_proc BY include.
*
*  CONCATENATE '(' p_prenew ')(.*)' INTO l_regex_find.
*  CONCATENATE '(' p_prenew ')(.)(.*)' INTO l_regex_find_include.
*
*  l_regex_change          = '$1SAPL$2'.
*
*  CONCATENATE 'L' p_preold '$3' INTO l_regex_change_include.
*
**  l_regex_change_include  = 'L$1$2'
*
*  l_programa = p_new.
*
*  REPLACE ALL OCCURRENCES OF REGEX l_regex_find
*                          IN l_programa
*                          WITH l_regex_change
*                          IGNORING CASE.
*
*  IF sy-subrc <> 0.
*    PERFORM f_add_msg USING c_msgid 'E' '802' text-t04 c_grupo_funcao
*     p_new 'Includes' '1'.
*    EXIT.
*  ENDIF.
*
*  CALL FUNCTION 'RS_GET_ALL_INCLUDES'
*    EXPORTING
*      program                      = l_programa
*     with_inactive_incls          = 'X'
**     WITH_RESERVED_INCLUDES       =
*    TABLES
*      includetab                   = lt_includes
*   EXCEPTIONS
*     not_existent                 = 1
*     no_program                   = 2
*            .
*
*  CHECK sy-subrc = 0.
*
*  LOOP AT lt_includes INTO lw_include.
*
*    CLEAR: lt_rssource[].
*
*    READ TABLE lt_includes_proc TRANSPORTING NO FIELDS WITH KEY include = lw_include BINARY SEARCH.
*
*    CHECK sy-subrc <> 0.
*
*    FIND REGEX l_regex_find
*                          IN lw_include
*                          IGNORING CASE.
*
*    CHECK sy-subrc = 0.
*
*    lw_include_orig = lw_include.
*
*    REPLACE ALL OCCURRENCES OF REGEX l_regex_find_include
*                        IN lw_include_orig
*                        WITH l_regex_change_include
*                        IGNORING CASE.
*
*    CHECK sy-subrc = 0.
*
*    "Normalizar o código do include para 72 posições por linha
*    PERFORM f_rs_source_split_to_72 USING lw_include_orig.
*
*    CHECK sy-subrc = 0.
*
*    "Lê o conteúdo do include da função
*    READ REPORT lw_include_orig INTO lt_rssource.
*
*    CHECK sy-subrc = 0.
*
*    PERFORM f_remover_linhas_include USING ' ' CHANGING lt_rssource.
*
*    INSERT REPORT lw_include FROM lt_rssource.
*
*  ENDLOOP.


ENDFORM.                    " F_COPIA_FUNCOES

*&---------------------------------------------------------------------*
*&      Form  F_REMOVER_LINHAS_INCLUDE
*&---------------------------------------------------------------------*
FORM f_remover_linhas_include USING p_remove_func CHANGING pt_code   TYPE yt_rssource.

  DATA: lw_code LIKE LINE OF pt_code,
        l_tabix TYPE sy-tabix.

  "Remove linhas do código fonte
  LOOP AT pt_code INTO lw_code.

    l_tabix = sy-tabix.

    IF p_remove_func IS NOT INITIAL.
      IF lw_code(8) = 'FUNCTION' OR lw_code CS 'ENDFUNCTION'.
        DELETE pt_code.
        CONTINUE.
      ELSEIF lw_code(2) = '*"'.
        DELETE pt_code.
        CONTINUE.
      ENDIF.
    ENDIF.

    REPLACE ALL OCCURRENCES OF REGEX gv_regex_module
                            IN lw_code
                            WITH gv_regex_change IGNORING CASE.

    IF sy-subrc <> 0.
      REPLACE ALL OCCURRENCES OF REGEX gv_regex_find
                              IN lw_code
                              WITH gv_regex_change IGNORING CASE.
    ENDIF.

    MODIFY pt_code FROM lw_code INDEX l_tabix.

  ENDLOOP.

ENDFORM.                    " F_REMOVER_LINHAS_INCLUDE

*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_TIPO_PARAMETER
*&---------------------------------------------------------------------*
FORM f_busca_tipo_parameter  CHANGING p_type TYPE rs38l_typ.

  DATA: l_tipo   TYPE string,
        lt_saida TYPE yt_string,
        lw_dtel  TYPE y_dtel,
        lw_tabl  TYPE y_tabl,
        lw_ttyp  TYPE y_ttyp.

  CHECK p_type IS NOT INITIAL.

  SPLIT p_type AT '-' INTO TABLE lt_saida.

  READ TABLE lt_saida INTO l_tipo INDEX 1.

  PERFORM f_buscar_ttyp USING l_tipo CHANGING lw_ttyp.
  IF sy-subrc = 0.
    l_tipo = lw_ttyp-new_name.
    MODIFY lt_saida FROM l_tipo INDEX 1.
    PERFORM f_montar_nome_saida USING lt_saida '-' CHANGING l_tipo.
    EXIT.
  ENDIF.

  PERFORM f_buscar_tabl USING l_tipo CHANGING lw_tabl.
  IF sy-subrc = 0.
    l_tipo = lw_tabl-new_name.
    MODIFY lt_saida FROM l_tipo INDEX 1.
    PERFORM f_montar_nome_saida USING lt_saida '-' CHANGING l_tipo.
    EXIT.
  ENDIF.

  PERFORM f_buscar_dtel USING l_tipo CHANGING lw_dtel.
  IF sy-subrc = 0.
    l_tipo = lw_dtel-new_name.
    MODIFY lt_saida FROM l_tipo INDEX 1.
    PERFORM f_montar_nome_saida USING lt_saida '-' CHANGING l_tipo.
    EXIT.
  ENDIF.

ENDFORM.                    " F_BUSCA_TIPO_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_DOMA
*&---------------------------------------------------------------------*
FORM f_buscar_doma  USING    p_name
                    CHANGING pw_obj TYPE y_doma.

  DATA: lw_mig_obj                TYPE zmigration_objs.

  CLEAR:  pw_obj,
          lw_mig_obj.

  READ TABLE gt_doma INTO pw_obj WITH KEY old_name = p_name BINARY
  SEARCH.

*  IF sy-subrc <> 0.
*
*    SELECT *
*      FROM zmigration_objs
*      INTO lw_mig_obj
*      UP TO 1 ROWS
*      WHERE object    = c_dominio
*        AND obj_name  = p_name.
*
*
*      pw_obj-old_name = lw_mig_obj-obj_name.
*      pw_obj-new_name = lw_mig_obj-new_name.
*      pw_obj-object   = lw_mig_obj-object.
*      APPEND pw_obj TO gt_doma.
*
*      SORT gt_doma BY old_name.
*
*    ENDSELECT.
*
*  ENDIF.

ENDFORM.                    " F_BUSCAR_DOMA

*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_DTEL
*&---------------------------------------------------------------------*
FORM f_buscar_dtel  USING    p_name
                    CHANGING pw_obj TYPE y_dtel.

  CLEAR: pw_obj.

  READ TABLE gt_dtel INTO pw_obj WITH KEY old_name = p_name BINARY
  SEARCH.

ENDFORM.                    " F_BUSCAR_DTEL

*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_TABL
*&---------------------------------------------------------------------*
FORM f_buscar_tabl  USING    p_name
                    CHANGING pw_obj TYPE y_tabl.

  CLEAR: pw_obj.

  READ TABLE gt_tabl INTO pw_obj WITH KEY old_name = p_name BINARY
  SEARCH.

ENDFORM.                    " F_BUSCAR_TABL

*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_TTYP
*&---------------------------------------------------------------------*
FORM f_buscar_ttyp  USING    p_name
                    CHANGING pw_obj TYPE y_ttyp.

  CLEAR: pw_obj.

  READ TABLE gt_ttyp INTO pw_obj WITH KEY old_name = p_name BINARY
  SEARCH.

ENDFORM.                    " F_BUSCAR_TTYP

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_NOME_SAIDA
*&---------------------------------------------------------------------*
FORM f_montar_nome_saida  USING    pt_parts     TYPE yt_string
                                   p_delimiter
                          CHANGING p_out.

  DATA: l_data   TYPE string,
        l_linha  TYPE i,
        l_linhas TYPE i.

  CLEAR: p_out.

  l_linhas = lines( pt_parts ).
  LOOP AT pt_parts INTO l_data.

    l_linha = sy-tabix.

    IF l_linha < l_linhas.
      CONCATENATE p_out p_delimiter l_data INTO p_out.
    ELSE.
      CONCATENATE p_out l_data INTO p_out.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_MONTAR_NOME_SAIDA

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_AJUDAS_PESQUISA
*&---------------------------------------------------------------------*
FORM f_copiar_ajudas_pesquisa .

  DATA: l_obj_src TYPE string,
        l_obj_new TYPE string,
        lw_obj    TYPE y_shlp.

  CHECK p_shlp IS NOT INITIAL.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS WITH KEY object =
  c_ajuda_pesquisa BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_ajuda_pesquisa.
      EXIT.
    ENDIF.

    l_obj_src = gw_tadir-obj_name.

    PERFORM f_novo_nome_obj USING gw_tadir-object l_obj_src CHANGING
    l_obj_new.

    IF l_obj_new IS INITIAL.
      "[REVISAR] Gravar log?
      PERFORM f_add_msg USING c_msgid 'E' '800' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    lw_obj-old_name = l_obj_src.
    lw_obj-new_name = l_obj_new.

    PERFORM f_dd_object_exists USING gw_tadir-object lw_obj-new_name
    CHANGING gv_existe.

    IF gv_existe IS NOT INITIAL.
      PERFORM f_add_msg USING c_msgid 'E' '801' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    PERFORM copia_ajuda_pesquisa  USING     lw_obj-old_name
                                                  lw_obj-new_name
                                                  p_pkgdes
                                                  gw_tadir
                                        CHANGING  lw_obj-info.

*    PERFORM f_copia_ajuda_pesquisa  USING     lw_obj-old_name
*                                              lw_obj-new_name
*                                              p_pkgdes
*                                    CHANGING  lw_obj-info.

*    IF lw_obj-info IS INITIAL.
*      "[REVISAR] Gravar log?
*      CONTINUE.
*    ENDIF.

    lw_obj-object = gw_tadir-object.

    APPEND lw_obj TO gt_shlp.

  ENDLOOP.

  SORT gt_shlp BY old_name.

ENDFORM.                    " F_COPIAR_AJUDAS_PESQUISA

*&---------------------------------------------------------------------*
*&      Form  F_GRAVAR_ALTERADOS
*&---------------------------------------------------------------------*
FORM f_gravar_alterados.

  FIELD-SYMBOLS: <fs_table>  TYPE STANDARD TABLE,
                 <fs_struct> TYPE any,
                 <fs_field>  TYPE any.

  DATA: lt_objs   TYPE yt_zmigration_objs,
        lw_obj    LIKE LINE OF lt_objs,
        lt_string TYPE yt_string WITH HEADER LINE,
        l_table   TYPE string.

  REFRESH: lt_string.
  lt_string = 'DOMA'. APPEND lt_string.
  lt_string = 'DTEL'. APPEND lt_string.
  lt_string = 'TABL'. APPEND lt_string.
  lt_string = 'TTYP'. APPEND lt_string.
  lt_string = 'VIEW'. APPEND lt_string.
  lt_string = 'FUGR'. APPEND lt_string.
  lt_string = 'SHLP'. APPEND lt_string.
  lt_string = 'PROG'. APPEND lt_string.
  lt_string = 'SSFO'. APPEND lt_string.
  lt_string = 'SSST'. APPEND lt_string.

  LOOP AT lt_string.

    UNASSIGN: <fs_table>,
              <fs_struct>.

    CONCATENATE 'GT_' lt_string INTO l_table.

    ASSIGN (l_table) TO <fs_table>.

    CHECK <fs_table> IS ASSIGNED.

    LOOP AT <fs_table> ASSIGNING <fs_struct>.
      CLEAR: lw_obj.

      lw_obj-devclass = p_pkgsrc.

      ASSIGN ('<FS_STRUCT>-OBJECT') TO <fs_field>.
      CHECK <fs_field> IS ASSIGNED.

      lw_obj-object = <fs_field>.

      ASSIGN ('<FS_STRUCT>-OLD_NAME') TO <fs_field>.
      CHECK <fs_field> IS ASSIGNED.
      lw_obj-obj_name = <fs_field>.

      ASSIGN ('<FS_STRUCT>-NEW_NAME') TO <fs_field>.
      CHECK <fs_field> IS ASSIGNED.
      lw_obj-new_name = <fs_field>.

      APPEND lw_obj TO lt_objs.

    ENDLOOP.

  ENDLOOP.

  CHECK lt_objs[] IS NOT INITIAL.

  MODIFY zmigration_objs FROM TABLE lt_objs.

ENDFORM.                    " F_GRAVAR_ALTERADOS

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_OBJETOS_ALTERADOS
*&---------------------------------------------------------------------*
FORM f_selecionar_objetos_alterados .

  FIELD-SYMBOLS: <fs_table>  TYPE STANDARD TABLE,
                 <fs_struct> TYPE any,
                 <fs_field>  TYPE any.

  DATA: lt_objs_excluidos TYPE yt_zmigration_objs,
        lw_obj            TYPE zmigration_objs,
        l_name            TYPE string,
        lr_wa             TYPE REF TO data,
        l_index           TYPE sy-tabix,
        lt_string         TYPE yt_string WITH HEADER LINE.

  SELECT *
    FROM zmigration_objs
    INTO TABLE gt_objs.

  CHECK sy-subrc = 0.

  SORT gt_objs BY devclass object obj_name.

  LOOP AT gt_objs INTO lw_obj.

    l_index = sy-tabix.

    PERFORM f_object_exit USING lw_obj CHANGING gv_existe.

    IF gv_existe IS INITIAL.
      APPEND lw_obj TO lt_objs_excluidos.
      DELETE gt_objs INDEX l_index.
      CONTINUE.
    ENDIF.

    UNASSIGN:
    <fs_table>,
    <fs_struct>,
    <fs_field>.

    CONCATENATE 'GT_' lw_obj-object INTO l_name.

    ASSIGN (l_name) TO <fs_table>.

    CHECK <fs_table> IS ASSIGNED.

    "Cria uma estrutura da tabela dinamicamente
    CREATE DATA lr_wa LIKE LINE OF <fs_table>.

    ASSIGN lr_wa->* TO <fs_struct>.

    CHECK <fs_struct> IS ASSIGNED.

    ASSIGN ('<FS_STRUCT>-OLD_NAME') TO <fs_field>.
    CHECK <fs_field> IS ASSIGNED.
    <fs_field> = lw_obj-obj_name.

    ASSIGN ('<FS_STRUCT>-NEW_NAME') TO <fs_field>.
    CHECK <fs_field> IS ASSIGNED.
    <fs_field> = lw_obj-new_name.

    ASSIGN ('<FS_STRUCT>-OBJECT') TO <fs_field>.
    CHECK <fs_field> IS ASSIGNED.
    <fs_field> = lw_obj-object.

    APPEND <fs_struct> TO <fs_table>.


  ENDLOOP.

  REFRESH: lt_string.
  lt_string = 'DOMA'. APPEND lt_string.
  lt_string = 'DTEL'. APPEND lt_string.
  lt_string = 'TABL'. APPEND lt_string.
  lt_string = 'TTYP'. APPEND lt_string.
  lt_string = 'VIEW'. APPEND lt_string.
  lt_string = 'FUGR'. APPEND lt_string.
  lt_string = 'SHLP'. APPEND lt_string.
  lt_string = 'PROG'. APPEND lt_string.
  lt_string = 'SSST'. APPEND lt_string.
  lt_string = 'SSFO'. APPEND lt_string.

  LOOP AT lt_string.
    CONCATENATE 'GT_' lt_string INTO l_name.

    ASSIGN (l_name) TO <fs_table>.

    CHECK <fs_table> IS ASSIGNED.

    SORT <fs_table> BY ('OLD_NAME') ('NEW_NAME').
  ENDLOOP.


  DELETE zmigration_objs FROM TABLE lt_objs_excluidos.

ENDFORM.                    " F_SELECIONAR_OBJETOS_ALTERADOS

*&---------------------------------------------------------------------*
*&      Form  F_CRIAR_LOG
*&---------------------------------------------------------------------*
FORM f_criar_log .

  DATA: l_log_header TYPE bal_s_log,
        l_log_handle TYPE balloghndl.

  "Criar log
  CREATE OBJECT go_log.

  l_log_header-object     = 'ZVTPR_LOG'.
  l_log_header-subobject  = 'ZVTPR_MIGRATION'.
  l_log_header-extnumber  = sy-repid.
  l_log_header-aldate     = sy-datum.
  l_log_header-altime     = sy-uzeit.
  l_log_header-aluser     = sy-uname.
  l_log_header-altcode    = sy-tcode.
  l_log_header-altcode    = sy-tcode.
  l_log_header-alprog     = sy-repid.

  CALL METHOD go_log->create_log_handle
    EXPORTING
      i_log_header            = l_log_header
      i_only_error            = ' '
    IMPORTING
      e_log_handle            = l_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_CRIAR_LOG

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_REFERENCIAS
*&---------------------------------------------------------------------*
FORM f_atualizar_referencias .

  FIELD-SYMBOLS: <fs_table>       TYPE STANDARD TABLE.

  DATA: lt_string  TYPE yt_string WITH HEADER LINE,
        l_formname TYPE string,
        l_table    TYPE string.

  CHECK p_doma    IS NOT INITIAL OR
        p_dtel    IS NOT INITIAL OR
        p_tabl    IS NOT INITIAL OR
        p_tabl_s  IS NOT INITIAL OR
        p_ttyp    IS NOT INITIAL OR
        p_shlp    IS NOT INITIAL.


  REFRESH: lt_string.
  lt_string = 'DOMA'. APPEND lt_string.
  lt_string = 'DTEL'. APPEND lt_string.
  lt_string = 'TABL'. APPEND lt_string.
  lt_string = 'TTYP'. APPEND lt_string.
  lt_string = 'SHLP'. APPEND lt_string.

  LOOP AT lt_string.
    CONCATENATE 'GT_' lt_string INTO l_table.

    ASSIGN (l_table) TO <fs_table>.

    CHECK <fs_table> IS ASSIGNED.

    SORT <fs_table> BY ('OLD_NAME') ('NEW_NAME').
  ENDLOOP.

  LOOP AT lt_string.

    CONCATENATE 'F_ATUALIZAR_REF_' lt_string INTO l_formname.

    PERFORM (l_formname) IN PROGRAM (sy-repid).

  ENDLOOP.

ENDFORM.                    " F_ATUALIZAR_REFERENCIAS

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_REF_DOMA
*&---------------------------------------------------------------------*
FORM f_atualizar_ref_doma .

  DATA: lw_obj  TYPE y_doma,
        lw_tabl TYPE y_tabl,
        l_index TYPE sy-tabix.


  LOOP AT gt_doma INTO lw_obj.

    l_index = sy-tabix.

    PERFORM f_ddif_doma_get USING lw_obj-new_name CHANGING lw_obj-info.

    CHECK lw_obj-info-entitytab IS NOT INITIAL.

    READ TABLE gt_tabl INTO lw_tabl WITH KEY old_name =
    lw_obj-info-entitytab BINARY SEARCH.

    CHECK sy-subrc = 0.

    lw_obj-info-entitytab = lw_tabl-new_name.

    PERFORM f_ddif_doma_put USING lw_obj-new_name lw_obj-info.

    IF sy-subrc <> 0.
      PERFORM f_add_msg USING c_msgid 'E' '802' text-t04 lw_tabl-object
      lw_tabl-new_name '' '1'.
      CONTINUE.
    ENDIF.

    PERFORM f_ddif_doma_activate USING lw_obj-new_name.

    "Conseguiu ativar o domínio.
    IF sy-subrc <> 0.
      PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 lw_obj-object
      lw_obj-new_name '' '1'.
      CONTINUE.
    ENDIF.

    MODIFY gt_doma FROM lw_obj INDEX l_index.

  ENDLOOP.


ENDFORM.                    " F_ATUALIZAR_REF_DOMA

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_REF_DTEL
*&---------------------------------------------------------------------*
FORM f_atualizar_ref_dtel .

  DATA: lw_obj  TYPE y_dtel,
        lw_doma TYPE y_doma,
        l_index TYPE sy-tabix.

  LOOP AT gt_dtel INTO lw_obj.

    l_index = sy-tabix.

    PERFORM f_ddif_dtel_get USING lw_obj-new_name CHANGING lw_obj-info.

    CHECK sy-subrc = 0.

    PERFORM f_buscar_doma USING lw_obj-info-domname CHANGING lw_doma.

    IF sy-subrc <> 0.
      MODIFY gt_dtel FROM lw_obj INDEX l_index.
      CONTINUE.
    ENDIF.

    lw_obj-info-domname = lw_doma-new_name.

    PERFORM f_ddif_dtel_put USING lw_obj-new_name lw_obj-info.

    IF sy-subrc <> 0.
      PERFORM f_add_msg USING c_msgid 'E' '802' text-t04 lw_obj-object
      lw_obj-new_name '' '1'.
      CONTINUE.
    ENDIF.

    PERFORM f_ddif_dtel_activate USING lw_obj-new_name.

    IF sy-subrc <> 0.
      PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 lw_obj-object
      lw_obj-new_name '' '1'.
      CONTINUE.
    ENDIF.

    PERFORM f_ddif_dtel_get USING lw_obj-new_name
                            CHANGING lw_obj-info.

    MODIFY gt_dtel FROM lw_obj INDEX l_index.

  ENDLOOP.

ENDFORM.                    " F_ATUALIZAR_REF_DTEL

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_REF_TABL
*&---------------------------------------------------------------------*
FORM f_atualizar_ref_tabl .

  DATA: lw_obj  TYPE y_tabl,
        l_index TYPE sy-tabix.

  DATA: lt_dd03p      TYPE dd03ttyp,
        lw_dd03p      TYPE dd03p,
        lt_dd05m      TYPE dd05mttyp,
        lw_dd05m      TYPE dd05m,
        lt_dd08v      TYPE dd08vttyp,
        lw_dd08v      TYPE dd08v,
        lt_dd12v      TYPE dd12vtab,
        lt_dd17v      TYPE dd17vtab,
        lt_dd35v      TYPE dd35vttyp,
        lw_dd35v      TYPE dd35v,
        lt_dd36m      TYPE dd36mttyp,
        lw_dd36m      TYPE dd36m,
        lw_ttyp       TYPE y_ttyp,
        lw_tabl       TYPE y_tabl,
        lw_dtel       TYPE y_dtel,
        lw_doma       TYPE y_doma,
        lw_shlp       TYPE y_shlp,
        l_index_field TYPE sy-tabix,
        l_alterado    TYPE c.


  LOOP AT gt_tabl INTO lw_obj.

    l_index = sy-tabix.

    CLEAR: l_alterado.

    PERFORM f_ddif_tabl_get USING     lw_obj-new_name
                            CHANGING  lw_obj-info
                                      lt_dd03p
                                      lt_dd05m
                                      lt_dd08v
                                      lt_dd12v
                                      lt_dd17v
                                      lt_dd35v
                                      lt_dd36m.

    CHECK sy-subrc = 0.

    PERFORM f_buscar_tabl USING lw_obj-info-sqltab CHANGING lw_tabl.

    IF sy-subrc = 0.
      lw_obj-info-sqltab = lw_tabl-new_name.
      l_alterado = 'X'.
    ENDIF.

    "Atualizar campos (Elemento de dados, domínios, estruturas, etc.. )
    LOOP AT lt_dd03p INTO lw_dd03p.


      l_index_field = sy-tabix.

      IF  lw_dd03p-fieldname = '.INCLUDE' OR
      "Se include ou campo faz parte de um include
          lw_dd03p-adminfield <> '0'.

        CASE lw_dd03p-comptype.
          WHEN 'S' OR 'L'.

            "Tenta encontrar a referência de uma estrutura
            PERFORM f_buscar_tabl USING lw_dd03p-precfield
                                        CHANGING lw_tabl.

            IF sy-subrc = 0.
              lw_dd03p-rollname = lw_tabl-new_name.
              l_alterado = 'X'.
            ENDIF.

          WHEN OTHERS.
        ENDCASE.

        MODIFY lt_dd03p FROM lw_dd03p INDEX l_index_field.

        CONTINUE.
      ENDIF.


      "Tenta encontrar a referência de uma categoria de tabela
      PERFORM f_buscar_ttyp USING lw_dd03p-rollname CHANGING lw_ttyp.
      IF sy-subrc = 0.
        lw_dd03p-rollname = lw_ttyp-new_name.
        l_alterado = 'X'.
      ENDIF.

      "Tenta encontrar a referência de uma estrutura
      PERFORM f_buscar_tabl USING lw_dd03p-rollname CHANGING lw_tabl.

      IF sy-subrc = 0.
        lw_dd03p-rollname = lw_tabl-new_name.
        l_alterado = 'X'.
      ENDIF.

      "Tenta encontrar a referência de um elemento de dados
      PERFORM f_buscar_dtel USING lw_dd03p-rollname CHANGING lw_dtel.

      IF sy-subrc = 0.
        lw_dd03p-rollname = lw_dtel-new_name.
        lw_dd03p-domname  = lw_dtel-info-domname.
*        lw_dd03p-domname3l
        l_alterado = 'X'.
      ENDIF.

      PERFORM f_buscar_tabl USING lw_dd03p-entitytab CHANGING lw_tabl.

      IF sy-subrc = 0.
        lw_dd03p-entitytab = lw_tabl-new_name.
        l_alterado = 'X'.
      ENDIF.

      PERFORM f_buscar_tabl USING lw_dd03p-checktable CHANGING lw_tabl.

      IF sy-subrc = 0.
        lw_dd03p-checktable = lw_tabl-new_name.
        l_alterado = 'X'.
      ENDIF.

*      "Tenta encontrar a referência de um domínio
*      PERFORM f_buscar_doma USING lw_dd03p-domname CHANGING lw_doma.
*
*      IF sy-subrc = 0.
*        lw_dd03p-domname = lw_doma-new_name.
*        l_alterado = 'X'.
*      ENDIF.

      MODIFY lt_dd03p FROM lw_dd03p INDEX l_index_field.

    ENDLOOP.

    LOOP AT lt_dd05m INTO lw_dd05m.

      l_index_field = sy-tabix.


      PERFORM f_buscar_tabl USING lw_dd05m-checktable CHANGING lw_tabl.

      IF sy-subrc = 0.
        lw_dd05m-checktable = lw_tabl-new_name.
        l_alterado = 'X'.
      ENDIF.

      MODIFY lt_dd05m FROM lw_dd05m INDEX l_index_field.

    ENDLOOP.

    LOOP AT lt_dd08v INTO lw_dd08v.

      l_index_field = sy-tabix.


      PERFORM f_buscar_tabl USING lw_dd08v-checktable CHANGING lw_tabl.

      IF sy-subrc = 0.
        lw_dd08v-checktable = lw_tabl-new_name.
        l_alterado = 'X'.
      ENDIF.

      MODIFY lt_dd08v FROM lw_dd08v INDEX l_index_field.

    ENDLOOP.


    "Atualiza search help
    LOOP AT lt_dd35v INTO lw_dd35v.
      l_index_field = sy-tabix.

      PERFORM f_buscar_shlp USING lw_dd35v-shlpname CHANGING lw_shlp.

      IF sy-subrc = 0.
        lw_dd35v-shlpname = lw_shlp-new_name.
        l_alterado = 'X'.
      ENDIF.

      MODIFY lt_dd35v FROM lw_dd35v INDEX l_index_field.
    ENDLOOP.

    "Atualiza campos do search help
    LOOP AT lt_dd36m INTO lw_dd36m.
      l_index_field = sy-tabix.

      PERFORM f_buscar_shlp USING lw_dd36m-shlpname CHANGING lw_shlp.

      IF sy-subrc = 0.
        lw_dd36m-shlpname = lw_shlp-new_name.
        l_alterado = 'X'.
      ENDIF.

      "Tenta encontrar a referência de um elemento de dados
      PERFORM f_buscar_dtel USING lw_dd36m-rollname CHANGING lw_dtel.

      IF sy-subrc = 0.
        lw_dd36m-rollname = lw_dtel-new_name.
        lw_dd36m-domname  = lw_dtel-info-domname.
*        lw_dd03p-domname3l
        l_alterado = 'X'.
      ENDIF.

      MODIFY lt_dd36m FROM lw_dd36m INDEX l_index_field.

    ENDLOOP.

    IF l_alterado IS NOT INITIAL.
      PERFORM f_ddif_tabl_put USING lw_obj-new_name lw_obj-info
                                                    lt_dd03p
                                                    lt_dd05m
                                                    lt_dd08v
                                                    lt_dd35v
                                                    lt_dd36m.

      IF sy-subrc <> 0.
        PERFORM f_add_msg USING c_msgid 'E' '802' text-t04 lw_obj-object
        lw_obj-new_name '' '1'.
        CONTINUE.
      ENDIF.

      PERFORM f_ddif_tabl_activate USING lw_obj-new_name.

      IF sy-subrc <> 0.
        PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 lw_obj-object
        lw_obj-new_name '' '1'.
      ENDIF.
    ENDIF.

    MODIFY gt_tabl FROM lw_obj INDEX l_index.

  ENDLOOP.

ENDFORM.                    " F_ATUALIZAR_REF_TABL

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_REF_TTYP
*&---------------------------------------------------------------------*
FORM f_atualizar_ref_ttyp .

  DATA: lw_obj  TYPE y_ttyp,
        lw_tabl TYPE y_tabl,
        l_index TYPE sy-tabix.

  LOOP AT gt_ttyp INTO lw_obj.

    l_index = sy-tabix.

    PERFORM f_ddif_ttyp_get USING lw_obj-new_name CHANGING lw_obj-info.

    CHECK sy-subrc = 0.

    "Se possui uma estrutura ou tabela que foi copiado/modificado
    PERFORM f_buscar_tabl USING lw_obj-info-rowtype CHANGING lw_tabl.

    CHECK sy-subrc = 0.

    lw_obj-info-rowtype = lw_tabl-new_name.

    PERFORM f_ddif_ttyp_put USING lw_obj-new_name lw_obj-info.

    IF sy-subrc <> 0.
      CLEAR: lw_tabl-new_name.
      PERFORM f_add_msg USING c_msgid 'E' '802' text-t04 lw_tabl-object
      lw_tabl-new_name '' '1'.
      CONTINUE.
    ENDIF.

    PERFORM f_ddif_ttyp_activate USING lw_obj-new_name.

    IF sy-subrc <> 0.
      PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 lw_obj-object
      lw_obj-new_name '' '1'.
      CONTINUE.
    ENDIF.

    MODIFY gt_ttyp FROM lw_obj INDEX l_index.

  ENDLOOP.

ENDFORM.                    " F_ATUALIZAR_REF_TTYP

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_REF_SHLP
*&---------------------------------------------------------------------*
FORM f_atualizar_ref_shlp .

  DATA: lw_obj  TYPE y_shlp,
        lw_tabl TYPE y_tabl,
        l_index TYPE sy-tabix.


  LOOP AT gt_shlp INTO lw_obj.

    l_index = sy-tabix.

    PERFORM f_ddif_shlp_get USING lw_obj-new_name CHANGING lw_obj-info.

    CHECK sy-subrc = 0.

    PERFORM f_buscar_tabl USING lw_obj-info-selmethod CHANGING lw_tabl.

    CHECK sy-subrc = 0.

    lw_obj-info-selmethod = lw_tabl-new_name.

    PERFORM f_ddif_shlp_put USING lw_obj-new_name lw_obj-info.

    IF sy-subrc <> 0.
      PERFORM f_add_msg USING c_msgid 'E' '802' text-t04 lw_tabl-object
      lw_tabl-new_name '' '1'.
      CONTINUE.
    ENDIF.

    PERFORM f_ddif_shlp_activate USING lw_obj-new_name.

    "Conseguiu ativar o domínio.
    IF sy-subrc <> 0.
      PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 lw_obj-object
      lw_obj-new_name '' '1'.
      CONTINUE.
    ENDIF.

    MODIFY gt_shlp FROM lw_obj INDEX l_index.

  ENDLOOP.

ENDFORM.                    " F_ATUALIZAR_REF_SHLP

*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_SHLP
*&---------------------------------------------------------------------*
FORM f_buscar_shlp  USING    p_name
                    CHANGING pw_obj TYPE y_shlp.

  CLEAR: pw_obj.

  READ TABLE gt_shlp INTO pw_obj WITH KEY old_name = p_name BINARY
  SEARCH.

ENDFORM.                    " F_BUSCAR_SHLP

"-----------------------------------------------------------------------
" Form copia_ajuda_pesquisa
"-----------------------------------------------------------------------
FORM copia_ajuda_pesquisa USING    p_src
                            p_new
                            p_devclass
                            p_tadir_old TYPE tadir " [INSERT] - MMSL- CH 9192 -  14.08.2017 17:28:46
                  CHANGING  p_dd30v     TYPE dd30v.

  DATA: lv_name TYPE ddobjname.

  lv_name = p_src.

  CALL FUNCTION 'DDIF_SHLP_GET'
    EXPORTING
      name          = lv_name
      state         = 'A'
      langu         = sy-langu
    IMPORTING
      dd30v_wa      = p_dd30v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    CLEAR: p_dd30v.
    EXIT.
  ENDIF.

  lv_name = p_new.

  p_dd30v-shlpname = lv_name.

  CALL FUNCTION 'DDIF_SHLP_PUT'
    EXPORTING
      name              = p_new
      dd30v_wa          = p_dd30v
    EXCEPTIONS
      shlp_not_found    = 1
      name_inconsistent = 2
      shlp_inconsistent = 3
      put_failure       = 4
      put_refused       = 5
      OTHERS            = 6.


  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_ajuda_pesquisa p_src
    '' '1'.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*
* > [BEGIN] MMSL - 9192 -  14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM insert_devclass USING p_devclass
                                p_tadir_old
                                lv_name.

  IF sy-subrc NE 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_ajuda_pesquisa p_new
          '' '1'.
    EXIT.
  ENDIF.
*----------------------------------------------------------------------*
* < [END] MMSL - 9192 - 14.08.2017 17:30:19
*----------------------------------------------------------------------*
  PERFORM f_ddif_doma_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_ajuda_pesquisa p_new
    '' '1'.
    EXIT.
  ENDIF.

ENDFORM. " copia_ajuda_pesquisa

*&---------------------------------------------------------------------*
*&      Form  F_COPIA_AJUDA_PESQUISA
*&---------------------------------------------------------------------*
FORM f_copia_ajuda_pesquisa USING     p_src
                                      p_new
                                      p_devclass
                            CHANGING  p_dd30v     TYPE dd30v.

  REFRESH:  gt_bdc,
          gt_msg.

  PERFORM cria_bdc_tab USING:
          'X' 'SAPMSRD0'           '0102',
          ' ' 'BDC_CURSOR'         'RSRD1-SHMA_VAL',
          ' ' 'BDC_OKCODE'         '=COPY',
          ' ' 'RSRD1-SHMA'         'X',
          ' ' 'RSRD1-SHMA_VAL'     p_src,

          'X' 'SAPLSDYY'           '0120',
          ' ' 'BDC_CURSOR'         '*RSEDD0-DDOBJNAME',
          ' ' 'BDC_OKCODE'         '=GOON',
          ' ' 'RSEDD0-DDOBJNAME'   p_src,
          ' ' '*RSEDD0-DDOBJNAME'  p_new,

          'X' 'SAPLSTRD'           '0100',
          ' ' 'BDC_CURSOR'         'KO007-L_DEVCLASS',
          ' ' 'BDC_OKCODE'         '=ADD',
          ' ' 'KO007-L_DEVCLASS'   p_devclass,
          ' ' 'KO007-L_AUTHOR'     sy-uname,

          'X' 'SAPLSTRD'           '0300',
          ' ' 'BDC_CURSOR'         'KO008-TRKORR',
          ' ' 'BDC_OKCODE'         '=LOCK',
          ' ' 'KO008-TRKORR'       p_reques.

  PERFORM cria_bdc_tab USING:
          'X' 'SAPMSRD0'           '0102',
          ' ' 'BDC_CURSOR'         'RSRD1-DDTYPE',
          ' ' 'BDC_OKCODE'         '=BACK',
          ' ' 'RSRD1-DDTYPE'       'X',
          ' ' 'RSRD1-DDTYPE_VAL'   p_new.

  CALL TRANSACTION 'SE11' USING gt_bdc          "Dados SHDB
                          MODE  gv_modo         "Modo execução
                          UPDATE 'S'            "Sincrona
                          MESSAGES INTO gt_msg. "Mensagens

  READ TABLE gt_msg TRANSPORTING NO FIELDS WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 c_ajuda_pesquisa
    p_src '' '1'.
    EXIT.
  ENDIF.

  PERFORM f_ddif_shlp_activate USING p_new.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03 c_ajuda_pesquisa
    p_new '' '1'.
    EXIT.
  ENDIF.

*  PERFORM f_ddif_shlp_get USING p_new CHANGING p_dd30v.

ENDFORM.                    " F_COPIA_AJUDA_PESQUISA

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_SHLP_ACTIVATE
*&---------------------------------------------------------------------*
FORM f_ddif_shlp_activate  USING    p_new.

  CALL FUNCTION 'DDIF_SHLP_ACTIVATE'
    EXPORTING
      name        = p_new
*     PRID        = -1
*   IMPORTING
*     RC          =
    EXCEPTIONS
      not_found   = 1
      put_failure = 2.

ENDFORM.                    " F_DDIF_SHLP_ACTIVATE

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_SHLP_GET
*&---------------------------------------------------------------------*
FORM f_ddif_shlp_get  USING    p_obj
                      CHANGING p_dd30v TYPE dd30v.

  CALL FUNCTION 'DDIF_SHLP_GET'
    EXPORTING
      name          = p_obj
      state         = 'M'
*     LANGU         = ' '
    IMPORTING
*     gotstate      =
      dd30v_wa      = p_dd30v
*   TABLES
*     DD31V_TAB     =
*     DD32P_TAB     =
*     DD33V_TAB     =
    EXCEPTIONS
      illegal_input = 1.

ENDFORM.                    " F_DDIF_SHLP_GET

*&---------------------------------------------------------------------*
*&      Form  F_DDIF_SHLP_PUT
*&---------------------------------------------------------------------*
FORM f_ddif_shlp_put  USING    p_obj
                               p_dd30v  TYPE dd30v.

  CALL FUNCTION 'DDIF_SHLP_PUT'
    EXPORTING
      name              = p_obj
      dd30v_wa          = p_dd30v
*   TABLES
*     DD31V_TAB         =
*     DD32P_TAB         =
*     DD33V_TAB         =
    EXCEPTIONS
      shlp_not_found    = 1
      name_inconsistent = 2
      shlp_inconsistent = 3
      put_failure       = 4
      put_refused       = 5.


ENDFORM.                    " F_DDIF_SHLP_PUT

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_TELAS
*&---------------------------------------------------------------------*
FORM f_copiar_telas .

  DATA: lt_screens    TYPE STANDARD TABLE OF d020s,
        lw_screen     TYPE d020s,
        l_progname    TYPE d020s-prog,
        lw_header     TYPE rpy_dyhead,
        lt_containers TYPE dycatt_tab,
        lt_fields     TYPE dyfatc_tab,
        lw_field      LIKE LINE OF lt_fields,
        lt_flow_logic TYPE STANDARD TABLE OF rpy_dyflow,
        lw_flow_logic LIKE LINE OF lt_flow_logic,
        lt_params     TYPE STANDARD TABLE OF rpy_dypara,
        lw_prog       TYPE y_prog,
        lw_shlp       TYPE y_shlp,
        l_struct      TYPE fieldname,
        l_field       TYPE fieldname,
        l_index       TYPE sy-tabix.

  CHECK p_screen IS NOT INITIAL.

  SORT gt_prog BY old_name new_name.

  LOOP AT gt_prog INTO lw_prog.

    l_progname = lw_prog-old_name.

    CALL FUNCTION 'BDT_SUPPL_GET_AUX_DYNPS'
      EXPORTING
        progname      = l_progname
      TABLES
        et_d020s      = lt_screens
      EXCEPTIONS
        nothing_found = 1.

    CHECK sy-subrc = 0.

    "Loop nas telas do programa
    LOOP AT lt_screens INTO lw_screen.

      CALL FUNCTION 'RPY_DYNPRO_READ'
        EXPORTING
          progname             = lw_screen-prog
          dynnr                = lw_screen-dnum
        IMPORTING
          header               = lw_header
        TABLES
          containers           = lt_containers
          fields_to_containers = lt_fields
          flow_logic           = lt_flow_logic
          params               = lt_params
        EXCEPTIONS
          cancelled            = 1
          not_found            = 2
          permission_error     = 3.

      CHECK sy-subrc = 0.

      "Novo nome do programa
      lw_header-program = lw_prog-new_name.

      "Loop dos campos da tela
      LOOP AT lt_fields INTO lw_field.

        CLEAR:  l_struct,
                l_field.

        l_index = sy-tabix.

        REPLACE ALL OCCURRENCES OF REGEX gv_regex_module
                            IN lw_field-name
                            WITH gv_regex_change IGNORING CASE.

        IF sy-subrc <> 0.
          REPLACE ALL OCCURRENCES OF REGEX gv_regex_find
                                  IN lw_field-name
                                  WITH gv_regex_change IGNORING CASE.
        ENDIF.

        "Procura por search help
        READ TABLE gt_shlp INTO lw_shlp
              WITH KEY old_name = lw_field-matchcode BINARY SEARCH.

        IF sy-subrc = 0.
          lw_field-matchcode = lw_shlp-new_name.
        ENDIF.

        MODIFY lt_fields FROM lw_field INDEX l_index.

      ENDLOOP.

      "Loop nas linhas da lógica da tela
      LOOP AT lt_flow_logic INTO lw_flow_logic.
        l_index = sy-tabix.

        REPLACE ALL OCCURRENCES OF REGEX gv_regex_module
                            IN lw_flow_logic-line
                            WITH gv_regex_change IGNORING CASE.

        IF sy-subrc <> 0.
          REPLACE ALL OCCURRENCES OF REGEX gv_regex_find
                                  IN lw_flow_logic-line
                                  WITH gv_regex_change IGNORING CASE.
        ENDIF.

        MODIFY lt_flow_logic FROM lw_flow_logic INDEX l_index.

      ENDLOOP.

      "Insere Tela no Programa Novo
      CALL FUNCTION 'RPY_DYNPRO_INSERT'
        EXPORTING
          suppress_generate      = 'X'
          header                 = lw_header
*         SUPPRESS_COMMIT_WORK   = ' '
        TABLES
          containers             = lt_containers
          fields_to_containers   = lt_fields
          flow_logic             = lt_flow_logic
          params                 = lt_params
        EXCEPTIONS
          cancelled              = 1
          already_exists         = 2
          program_not_exists     = 3
          not_executed           = 4
          missing_required_field = 5
          illegal_field_value    = 6
          field_not_allowed      = 7
          not_generated          = 8
          illegal_field_position = 9.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " F_COPIAR_TELAS

*&---------------------------------------------------------------------*
*&      Form  F_RS_SOURCE_SPLIT_TO_72
*&---------------------------------------------------------------------*
FORM f_rs_source_split_to_72  USING    p_progname.

  SUBMIT rs_source_split_to_72
        WITH progname = p_progname
        AND RETURN.

ENDFORM.                    " F_RS_SOURCE_SPLIT_TO_72

*&---------------------------------------------------------------------*
*&      Form  F_GRUPO_FUNCAO_SM30
*&---------------------------------------------------------------------*
FORM f_grupo_funcao_sm30  USING    p_src
                          CHANGING p_sm30.

  DATA: lt_funcoes  TYPE STANDARD TABLE OF rs38l_incl,
        lw_funcao   TYPE rs38l_incl,
        l_func_sm30 TYPE i,
        l_funcoes   TYPE i.

  CLEAR: p_sm30.

  "Busca todas as funções que o grupo de funções possui
  CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
    EXPORTING
      function_pool           = p_src
    TABLES
      functab                 = lt_funcoes
    EXCEPTIONS
      function_pool_not_found = 1.

  CHECK sy-subrc = 0.

  CLEAR: l_func_sm30.

  LOOP AT lt_funcoes INTO lw_funcao.

    IF  lw_funcao-funcname CS 'TABLEFRAME' OR
        lw_funcao-funcname CS 'TABLEPROC_' OR
        lw_funcao-funcname CS 'VIEWFRAME' OR
        lw_funcao-funcname CS 'VIEWPROC_'.

      ADD 1 TO l_func_sm30.

    ENDIF.

  ENDLOOP.

  l_funcoes = lines( lt_funcoes ).

  IF l_func_sm30 = l_funcoes.
    p_sm30 = 'X'.
  ENDIF.

ENDFORM.                    " F_GRUPO_FUNCAO_SM30

*&---------------------------------------------------------------------*
*&      Form  F_OBJECT_EXIT
*&---------------------------------------------------------------------*
FORM f_object_exit  USING    pw_obj     TYPE zmigration_objs
                    CHANGING p_existe.

  DATA: lw_trdir                  TYPE trdir.

  CASE pw_obj-object.
    WHEN 'DOMA' OR 'DTEL' OR 'TABL' OR 'TTYP' OR 'VIEW'.
      PERFORM f_dd_object_exists USING pw_obj-object pw_obj-new_name
            CHANGING p_existe.
    WHEN 'FUGR'.
      PERFORM f_function_pool_exists USING    pw_obj-devclass
                                              pw_obj-new_name
                                     CHANGING p_existe.
    WHEN 'PROG'.
      PERFORM f_swy_program_exists  USING     pw_obj-new_name
                                    CHANGING  p_existe
                                              lw_trdir.
    WHEN OTHERS.
      p_existe = 'X'.
  ENDCASE.

ENDFORM.                    " F_OBJECT_EXIT

*&---------------------------------------------------------------------*
*&      Form  F_SWY_PROGRAM_EXISTS
*&---------------------------------------------------------------------*
FORM f_swy_program_exists  USING    p_progname
                           CHANGING p_existe
                                    pw_trdir    TYPE trdir.

  DATA: l_program                 TYPE swyprog-prognam.

  l_program = p_progname.

  CLEAR: p_existe.

  CALL FUNCTION 'SWY_PROGRAM_EXISTS'
    EXPORTING
      program            = l_program
    IMPORTING
      trdir_entry        = pw_trdir
    EXCEPTIONS
      program_not_exists = 1.

  IF sy-subrc = 0.
    p_existe = 'X'.
  ENDIF.

ENDFORM.                    " F_SWY_PROGRAM_EXISTS

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_PROGRAMAS_INCLUDES
*&---------------------------------------------------------------------*
FORM f_copiar_programas_includes .

  DATA: l_obj_src TYPE string,
        l_obj_new TYPE string,
        lw_obj    TYPE y_prog,
        lw_trdir  TYPE trdir.

  CHECK p_prog IS NOT INITIAL.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS
  WITH KEY object = c_programa
  BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_programa.
      EXIT.
    ENDIF.

    l_obj_src = gw_tadir-obj_name.

    PERFORM f_swy_program_exists  USING   l_obj_src
                                CHANGING  gv_existe
                                          lw_trdir.

    CHECK gv_existe IS NOT INITIAL.

    CHECK lw_trdir-subc = 'I' OR lw_trdir-subc = 'M' OR
          lw_trdir-subc = '1'.

    PERFORM f_novo_nome_prog USING l_obj_src CHANGING l_obj_new.

    IF l_obj_new IS INITIAL.

      "[REVISAR] Gravar log?
      PERFORM f_add_msg USING c_msgid 'E' '800' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.

*    ELSEIF  l_obj_src(1) <> 'Z'    OR
**            l_obj_src(1) <> '/'    OR
*            l_obj_src(4) <> 'SAPM'.
*
*     PERFORM f_add_msg USING c_msgid 'E' '802' text-t02 gw_tadir-object
*     gw_tadir-obj_name '' '1'.
*      CONTINUE.

    ENDIF.

    lw_obj-old_name = l_obj_src.
    lw_obj-new_name = l_obj_new.

    PERFORM f_swy_program_exists  USING     lw_obj-new_name
                                  CHANGING  gv_existe
                                            lw_trdir.

    IF gv_existe IS NOT INITIAL.
      PERFORM f_add_msg USING c_msgid 'E' '801' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    PERFORM f_copia_programa_include USING    lw_obj
                                              p_pkgdes.


    PERFORM f_swy_program_exists  USING     lw_obj-new_name
                              CHANGING  gv_existe
                                        lw_trdir.

    CHECK gv_existe IS NOT INITIAL.

    lw_obj-object = gw_tadir-object.
    APPEND lw_obj TO gt_prog.

  ENDLOOP.

  SORT gt_prog BY old_name.

ENDFORM.                    " F_COPIAR_PROGRAMAS_INCLUDES

*&---------------------------------------------------------------------*
*&      Form  F_COPIA_PROGRAMA_INCLUDE
*&---------------------------------------------------------------------*
FORM f_copia_programa_include  USING    pw_obj      TYPE y_prog
                                        p_devclass.

  DATA: l_program   TYPE sy-repid,
        l_source    TYPE sy-repid,
        l_subrc     TYPE sy-subrc,
        lt_code_src TYPE STANDARD TABLE OF string
                                  WITH HEADER LINE,
        lt_code_des TYPE STANDARD TABLE OF string
                                  WITH HEADER LINE,
        l_include   TYPE string,
        l_existe    TYPE c,
        lw_trdir    TYPE trdir.

  PERFORM f_swy_program_exists  USING     pw_obj-old_name
                                CHANGING  l_existe
                                          lw_trdir.

  CHECK sy-subrc = 0.

  l_program = pw_obj-new_name.
  l_source  = pw_obj-old_name.
  CONCATENATE p_prenew 'm' INTO l_include.

  CASE lw_trdir-subc.
    WHEN 'I'. "Include

      CALL FUNCTION 'RS_COPY_PROGRAM_INCLUDE'
        EXPORTING
          corrnumber         = p_reques
          devclass           = p_devclass
          program            = l_program
          source_program     = l_source
          suppress_popup     = 'X'
          suppress_screen    = 'X'
        EXCEPTIONS
          enqueue_lock       = 1
          object_not_found   = 2
          permission_failure = 3
          reject_copy        = 4.

    WHEN OTHERS.

      CALL FUNCTION 'RS_COPY_PROGRAM'
        EXPORTING
          corrnumber         = p_reques
          devclass           = p_devclass
          program            = l_program
          source_program     = l_source
          suppress_popup     = 'X'
          suppress_screen    = 'X'
          with_cua           = 'X'
          with_documentation = 'X'
          with_dynpro        = ' '
          with_includes      = ' '
          with_textpool      = 'X'
          with_variants      = 'X'
          generated          = 'X'
        EXCEPTIONS
          enqueue_lock       = 1
          object_not_found   = 2
          permission_failure = 3
          reject_copy        = 4.

  ENDCASE.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802'
                            text-t02
                            pw_obj-object
                            pw_obj-old_name
                            ''
                            '1'.
    EXIT.
  ENDIF.

  WAIT UP TO '0.5' SECONDS.

  "Ativa o programa
  PERFORM f_reps_object_activate  USING pw_obj-object pw_obj-new_name
                                  CHANGING l_subrc.

  IF l_subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t03
                            pw_obj-object
                            pw_obj-new_name
                            ''
                            '1'.
    EXIT.
  ENDIF.

  READ REPORT pw_obj-new_name INTO lt_code_src.

  IF sy-subrc <> 0.
    PERFORM f_add_msg USING c_msgid 'E' '802' text-t04
                            pw_obj-object
                            pw_obj-new_name
                            ''
                            '1'.
    EXIT.
  ENDIF.

  LOOP AT lt_code_src.

    REPLACE ALL OCCURRENCES OF REGEX gv_regex_module
                            IN lt_code_src
                            WITH gv_regex_change IGNORING CASE.

    IF sy-subrc <> 0.
      REPLACE ALL OCCURRENCES OF REGEX gv_regex_find
                              IN lt_code_src
                              WITH gv_regex_change IGNORING CASE.
    ENDIF.

    APPEND lt_code_src TO lt_code_des.

  ENDLOOP.

  INSERT REPORT pw_obj-new_name FROM lt_code_des.

ENDFORM.                    " F_COPIA_PROGRAMA_INCLUDE

*&---------------------------------------------------------------------*
*&      Form  F_REPS_OBJECT_ACTIVATE
*&---------------------------------------------------------------------*
FORM f_reps_object_activate  USING    p_obj_name
                                      p_object
                             CHANGING p_subrc.

  DATA: l_progname                TYPE e071-obj_name.

  l_progname = p_obj_name.

  CALL FUNCTION 'REPS_OBJECT_ACTIVATE'
    EXPORTING
      object_name  = l_progname
      object_type  = 'REPS'
    EXCEPTIONS
      not_executed = 1.

  p_subrc = sy-subrc.

ENDFORM.                    " F_REPS_OBJECT_ACTIVATE

*&---------------------------------------------------------------------*
*&      Form  F_NOVO_NOME_PROG
*&---------------------------------------------------------------------*
FORM f_novo_nome_prog  USING    p_src
                       CHANGING p_new.

  CONSTANTS: c_max_length        TYPE i VALUE 30.

  DATA: l_length                  TYPE i.

  p_new = p_src.

  REPLACE ALL OCCURRENCES OF REGEX gv_regex_module
                              IN p_new
                              WITH gv_regex_change IGNORING CASE.

  IF sy-subrc <> 0.
    REPLACE ALL OCCURRENCES OF REGEX gv_regex_find
                            IN p_new
                            WITH gv_regex_change IGNORING CASE.
  ENDIF.

  l_length = strlen( p_new ).

  IF l_length > c_max_length.
    CLEAR: p_new.
  ENDIF.

ENDFORM.                    " F_NOVO_NOME_PROG

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_DADOS_TAB
*&---------------------------------------------------------------------*
FORM f_copiar_dados_tab  USING    p_old
                                  p_new.

  DATA: lo_struct     TYPE REF TO cl_abap_structdescr,
        lo_struct_new TYPE REF TO cl_abap_structdescr,
        lt_comps      TYPE abap_component_tab,
        lt_comps_new  TYPE abap_component_tab,
        lo_new_tab    TYPE REF TO cl_abap_tabledescr,
        lo_data       TYPE REF TO data,
        l_where       TYPE string,
        l_display     TYPE c,
        l_state       TYPE ddgotstate,
        l_dd02v       TYPE dd02v.

  FIELD-SYMBOLS: <fs_table>     TYPE STANDARD TABLE.

  UNASSIGN: <fs_table>.

  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = p_new
*     STATE         = 'A'
*     LANGU         = ' '
    IMPORTING
      gotstate      = l_state
      dd02v_wa      = l_dd02v
*     DD09L_WA      =
*   TABLES
*     DD03P_TAB     =
*     DD05M_TAB     =
*     DD08V_TAB     =
*     DD12V_TAB     =
*     DD17V_TAB     =
*     DD35V_TAB     =
*     DD36M_TAB     =
    EXCEPTIONS
      illegal_input = 1.

  CHECK sy-subrc = 0  AND
        l_state = 'A' AND
        l_dd02v-contflag IS NOT INITIAL. "Possui classe de entrega

  lo_struct     ?= cl_abap_typedescr=>describe_by_name( p_old ).

  CHECK lo_struct IS NOT INITIAL.

  lo_struct_new ?= cl_abap_typedescr=>describe_by_name( p_new ).

  CHECK lo_struct_new IS NOT INITIAL.

  lt_comps      = lo_struct->get_components( ).
  lt_comps_new  = lo_struct_new->get_components( ).

  IF  lines( lt_comps ) <>
      lines( lt_comps_new ).

    PERFORM f_add_msg USING c_msgid 'E' '803' p_old
    p_new '' '' '1'.
    EXIT.
  ENDIF.

  lo_new_tab = cl_abap_tabledescr=>create(
  p_line_type  = lo_struct
  p_table_kind = cl_abap_tabledescr=>tablekind_std
  p_unique     = abap_false ).

  CREATE DATA lo_data TYPE HANDLE lo_new_tab.
  ASSIGN lo_data->* TO <fs_table>.

  CHECK <fs_table> IS ASSIGNED.

  SELECT *
  FROM (p_old)
  INTO CORRESPONDING FIELDS OF TABLE <fs_table>.

  CHECK sy-subrc = 0.

  MODIFY (p_new) FROM TABLE <fs_table>.

ENDFORM.                    " F_COPIAR_DADOS_TAB

*&---------------------------------------------------------------------*
*&      Form  F_GERAR_VISAO_ATUALIZACAO
*&---------------------------------------------------------------------*
FORM f_gerar_visao_atualizacao .

  DATA: lt_tvdir TYPE STANDARD TABLE OF tvdir,
        lw_tvdir TYPE tvdir,
        l_field  TYPE string,
        l_table  TYPE string,
        l_grupo  TYPE string.

  CHECK p_upview IS NOT INITIAL.

  SELECT *
    FROM tvdir
    INTO TABLE lt_tvdir
    WHERE devclass = p_pkgsrc.

  CHECK sy-subrc = 0.

  LOOP AT lt_tvdir INTO lw_tvdir.

    l_table = lw_tvdir-tabname.
    l_grupo = lw_tvdir-area.

    REPLACE ALL OCCURRENCES OF REGEX gv_regex_module
                        IN l_table
                        WITH gv_regex_change IGNORING CASE.

    IF sy-subrc <> 0.
      REPLACE ALL OCCURRENCES OF REGEX gv_regex_find
                              IN l_table
                              WITH gv_regex_change IGNORING CASE.
    ENDIF.

    REPLACE ALL OCCURRENCES OF REGEX gv_regex_module
                    IN l_grupo
                    WITH gv_regex_change IGNORING CASE.

    IF sy-subrc <> 0.
      REPLACE ALL OCCURRENCES OF REGEX gv_regex_find
                              IN l_grupo
                              WITH gv_regex_change IGNORING CASE.
    ENDIF.

    REFRESH: gt_bdc.

    CONCATENATE 'VIMDYNFLDS-MTYPE' lw_tvdir-type INTO l_field.
    CONDENSE l_field NO-GAPS.

    PERFORM cria_bdc_tab USING:
        'X' 'SAPMSRD0'              '0102',
        ''  'BDC_CURSOR'            'RSRD1-TBMA_VAL',
        ''  'BDC_OKCODE'            '=TMGE',
        ''  'RSRD1-TBMA'            'X',
        ''  'RSRD1-TBMA_VAL'        l_table,

        'X' 'SAPMSVIM'              '0120',
        ''  'BDC_CURSOR'            'TVDIR-DETAIL',
        ''  'BDC_OKCODE'            '=GENE',
        ''  'TDDAT-CCLASS'          '&NC&',
        ''  'TVDIR-AREA'            l_grupo,
        ''  l_field                  'X',
        ''  'TVDIR-LISTE'            lw_tvdir-liste,
        ''  'TVDIR-DETAIL'          lw_tvdir-detail,
        ''  'VIMDYNFLDS-CORR_CON_S'  'X',

        'X' 'SAPLSTRD'              '0100',
        ''  'BDC_CURSOR'            'KO007-L_DEVCLASS',
        ''  'BDC_OKCODE'            '=ADD',
        ''  'KO007-L_DEVCLASS'      p_pkgdes,
        ''  'KO007-L_AUTHOR'        sy-uname,

        'X' 'SAPLSTRD'              '0100',
        ''  'BDC_CURSOR'            'KO007-L_DEVCLASS',
        ''  'BDC_OKCODE'            '=ADD',
        ''  'KO007-L_DEVCLASS'      p_pkgdes,
        ''  'KO007-L_AUTHOR'        sy-uname,

        ''  'SAPLSTRD'              '0300',
        ''  'BDC_CURSOR'            'KO008-TRKORR',
        ''  'BDC_OKCODE'            '=LOCK',
        ''  'KO008-TRKORR'          p_reques,

        'X' 'SAPLSTRD'              '0100',
        ''  'BDC_CURSOR'            'KO007-L_DEVCLASS',
        ''  'BDC_OKCODE'            '=ADD',
        ''  'KO007-L_DEVCLASS'      p_pkgdes,
        ''  'KO007-L_AUTHOR'        sy-uname,

        'X' 'SAPMSSY0'              '0120',
        ''  'BDC_OKCODE'            '=&F03'.

    CALL TRANSACTION 'SE11' USING   gt_bdc      "Dados SHDB
                            MODE    gv_modo     "Modo execução
                            UPDATE 'S'         "Sincrona
                            MESSAGES INTO gt_msg. "Mensagens

  ENDLOOP.

ENDFORM.                    " F_GERAR_VISAO_ATUALIZACAO

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_CONTEUDO_TABELAS
*&---------------------------------------------------------------------*
FORM f_copiar_conteudo_tabelas .

  DATA: lw_tabl                   TYPE y_tabl.

  CHECK p_coptab IS NOT INITIAL.

  LOOP AT gt_tabl INTO lw_tabl.

    READ TABLE gt_tadir INTO gw_tadir WITH KEY obj_name = lw_tabl-old_name BINARY SEARCH.

    CHECK sy-subrc = 0. "Garante que copiará dados das tabelas apenas do pacote de origem selecionado

    PERFORM f_copiar_dados_tab USING lw_tabl-old_name lw_tabl-new_name.
  ENDLOOP.

ENDFORM.                    " F_COPIAR_CONTEUDO_TABELAS

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_SMARTFORMS
*&---------------------------------------------------------------------*
FORM f_copiar_smartforms .

  DATA: l_obj_src TYPE tdsfname,
        l_obj_new TYPE tdsfname,
        lw_obj    TYPE y_ssfo.

  CHECK p_ssfo IS NOT INITIAL.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS
        WITH KEY object = c_smartforms BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_smartforms.
      EXIT.
    ENDIF.

    l_obj_src = gw_tadir-obj_name.

    PERFORM f_novo_nome_ssf USING gw_tadir-object l_obj_src CHANGING
    l_obj_new.

    IF l_obj_new IS INITIAL.
      "[REVISAR] Gravar log?
      PERFORM f_add_msg USING c_msgid 'E' '800' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    PERFORM f_copia_smartform USING   gw_tadir
                                      p_pkgdes
                                      l_obj_src
                                      l_obj_new.

    CHECK sy-subrc = 0.

    lw_obj-old_name = l_obj_src.
    lw_obj-new_name = l_obj_new.
    lw_obj-object   = gw_tadir-object.

    APPEND lw_obj TO gt_ssfo.

  ENDLOOP.

  SORT gt_ssfo BY old_name.

ENDFORM.                    " F_COPIAR_SMARTFORMS

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_SMARTSTYLES
*&---------------------------------------------------------------------*
FORM f_copiar_smartstyles .

  DATA: l_obj_src TYPE tdssname,
        l_obj_new TYPE tdssname,
        lw_obj    TYPE y_ssst.

  CHECK p_ssst IS NOT INITIAL.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS
        WITH KEY object = c_smartstyle BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_smartstyle.
      EXIT.
    ENDIF.

    l_obj_src = gw_tadir-obj_name.

    PERFORM f_novo_nome_ssf USING gw_tadir-object l_obj_src CHANGING
    l_obj_new.

    IF l_obj_new IS INITIAL.
      "[REVISAR] Gravar log?
      PERFORM f_add_msg USING c_msgid 'E' '800' gw_tadir-object
      gw_tadir-obj_name '' '' '1'.
      CONTINUE.
    ENDIF.

    PERFORM f_copia_smartstyle USING  gw_tadir
                                      p_pkgdes
                                      l_obj_src
                                      l_obj_new.

    lw_obj-old_name = l_obj_src.
    lw_obj-new_name = l_obj_new.
    lw_obj-object   = gw_tadir-object.

    APPEND lw_obj TO gt_ssst.

  ENDLOOP.

  SORT gt_ssst BY old_name.

ENDFORM.                    " F_COPIAR_SMARTSTYLES

*&---------------------------------------------------------------------*
*&      Form  F_COPIA_SMARTFORM
*&---------------------------------------------------------------------*
FORM f_copia_smartform USING      p_tadir TYPE tadir
                                  p_devclass
                                  p_src
                                  p_new.

  DATA: l_form       TYPE REF TO cl_ssf_fb_smart_form,
        l_lock       TYPE c,
        sf_exception TYPE REF TO cx_ssf_fb,
        lw_interface TYPE ssfiopar,
        lw_data      TYPE ssfgdata,
        l_index      TYPE sy-tabix.

  REFRESH:  gt_bdc,
            gt_msg.

  PERFORM f_checar_request_obj USING    tadir-pgmid
                                        tadir-object
                                        p_new
                               CHANGING l_lock.

  PERFORM cria_bdc_tab USING:
      'X' 'SAPMSSFO'              '0100',
      ' ' 'BDC_CURSOR'            'SSFSCREEN-FNAME',
      ' ' 'BDC_OKCODE'            '=COPY',
      ' ' 'RB_SF'                 'X',
      ' ' 'SSFSCREEN-FNAME'       p_src,

      'X' 'SAPLSTXB'              '3010',
      ' ' 'BDC_CURSOR'            'SSFSCREEN-FNAME_NEW',
      ' ' 'BDC_OKCODE'            '=ENTER',
      ' ' 'SSFSCREEN-FNAME'       p_src,
      ' ' 'SSFSCREEN-FNAME_NEW'   p_new.

  IF l_lock IS INITIAL.
    PERFORM cria_bdc_tab USING:
        'X' 'SAPLSTRD'              '0100',
        ' ' 'BDC_CURSOR'            'KO007-L_DEVCLASS',
        ' ' 'BDC_OKCODE'            '=ADD',
        ' ' 'KO007-L_DEVCLASS'      p_devclass,
        ' ' 'KO007-L_AUTHOR'        sy-uname,

        'X' 'SAPLSTRD'              '0300',
        ' ' 'BDC_CURSOR'            'KO008-TRKORR',
        ' ' 'BDC_OKCODE'            '=LOCK',
        ' ' 'KO008-TRKORR'          p_reques.
  ENDIF.

  CALL TRANSACTION 'SMARTFORMS'
                          USING   gt_bdc        "Dados SHDB
                          MODE    gv_modo       "Modo execução
                          UPDATE  'S'           "Sincrona
                          MESSAGES INTO gt_msg. "Mensagens

  READ TABLE gt_msg TRANSPORTING NO FIELDS WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
    PERFORM f_add_msg USING c_msgid 'E'  '802'
                                         text-t02
                                         p_tadir-object
                                         p_src
                                         ''
                                         '1'.
    EXIT.
  ENDIF.

  CREATE OBJECT l_form.

  TRY.
      CALL METHOD l_form->load
        EXPORTING
          im_formname = p_new.

      CALL METHOD l_form->enqueue
        EXPORTING
*         authority_check         = 'X'
*         suppress_corr_check     = SPACE
*         suppress_language_check = 'X'
*         language_upd_exit       = SPACE
*         master_language         = SPACE
*         mode     = 'MODIFY'
          formname = p_new
*        IMPORTING
*         devclass =
*         new_master_language     =
*         korrnum  =
*         modification_language   =
        .

    CATCH cx_ssf_fb INTO sf_exception.
      EXIT.
  ENDTRY.

  LOOP AT l_form->interface INTO lw_interface.

    l_index = sy-tabix.

    REPLACE ALL OCCURRENCES OF REGEX gv_regex_find
                        IN lw_interface-typename
                        WITH gv_regex_change IGNORING CASE.

    MODIFY l_form->interface FROM lw_interface INDEX l_index.

  ENDLOOP.

  LOOP AT l_form->gdata INTO lw_data.

    l_index = sy-tabix.

    REPLACE ALL OCCURRENCES OF REGEX gv_regex_find
                        IN lw_data-typename
                        WITH gv_regex_change IGNORING CASE.

    MODIFY l_form->gdata FROM lw_data INDEX l_index.

  ENDLOOP.

  TRY.

      l_form->store(
        im_formname = p_new
        im_active   = 'X' ).

      l_form->dequeue(
        formname = p_new ).

    CATCH cx_ssf_fb INTO sf_exception.
      EXIT.
  ENDTRY.

ENDFORM.                    " F_COPIA_SMARTFORM

*&---------------------------------------------------------------------*
*&      Form  F_COPIA_SMARTSTYLE
*&---------------------------------------------------------------------*
FORM f_copia_smartstyle  USING    p_tadir TYPE tadir
                                  p_devclass
                                  p_src
                                  p_new.

  DATA: l_lock                    TYPE c.

  REFRESH:  gt_bdc,
            gt_msg.

  PERFORM f_checar_request_obj USING    tadir-pgmid
                                        tadir-object
                                        p_new
                               CHANGING l_lock.


  PERFORM cria_bdc_tab USING:
      'X' 'SAPMSSFS'              '0100',
      ' ' 'BDC_CURSOR'            'SSFSCREENS-SNAME',
      ' ' 'BDC_OKCODE'            '=COPY',
      ' ' 'SSFSCREENS-SNAME'      p_src,

      'X' 'SAPLSTXBS'             '0140',
      ' ' 'BDC_CURSOR'            'SSFSCREENS-SNAME_NEW',
      ' ' 'BDC_OKCODE'            '=ENTER',
      ' ' 'SSFSCREENS-SNAME'      p_src,
      ' ' 'SSFSCREENS-SNAME_NEW'  p_new.

  IF l_lock IS INITIAL.
    PERFORM cria_bdc_tab USING:
        'X' 'SAPLSTRD'              '0100',
        ' ' 'BDC_CURSOR'            'KO007-L_DEVCLASS',
        ' ' 'BDC_OKCODE'            '=ADD',
        ' ' 'KO007-L_DEVCLASS'      p_devclass,
        ' ' 'KO007-L_AUTHOR'        sy-uname,

        'X' 'SAPLSTRD'              '0300',
        ' ' 'BDC_CURSOR'            'KO008-TRKORR',
        ' ' 'BDC_OKCODE'            '=LOCK',
        ' ' 'KO008-TRKORR'          p_reques.
  ENDIF.

  CALL TRANSACTION 'SMARTSTYLES'
                          USING   gt_bdc        "Dados SHDB
                          MODE    gv_modo       "Modo execução
                          UPDATE  'S'           "Sincrona
                          MESSAGES INTO gt_msg. "Mensagens

  CHECK sy-subrc = 0.

  CALL FUNCTION 'SSF_ACTIVATE_STYLE'
    EXPORTING
      i_stylename          = p_new
*     I_WITH_DIALOG        = ' '
*     REDIRECT_ERROR_MSG   = ' '
*   IMPORTING
*     O_STYLENAME          =
*     O_STATUS             =
*   TABLES
*     ERROR_MSG            =
    EXCEPTIONS
      no_name              = 1
      no_style             = 2
      cancelled            = 3
      no_access_permission = 4
      illegal_language     = 5.


ENDFORM.                    " F_COPIA_SMARTSTYLE

*&---------------------------------------------------------------------*
*&      Form  F_CHECAR_REQUEST_OBJ
*&---------------------------------------------------------------------*
FORM f_checar_request_obj  USING    p_pgmid   TYPE e071-pgmid
                                    p_object  TYPE e071-object
                                    p_name
                           CHANGING p_lock.

  DATA: lt_locks TYPE STANDARD TABLE OF tlock,
        l_name   TYPE e071-obj_name,
        l_locked TYPE trpari-s_checked.

  CLEAR: p_lock.

  l_name = p_name.

  CALL FUNCTION 'TR_CHECK_OBJECT_LOCK'
    EXPORTING
      wi_pgmid             = p_pgmid
      wi_object            = p_object
      wi_objname           = l_name
*     IT_TLOCK_ENTRIES     =
    IMPORTING
*     WE_LOCKABLE_OBJECT   =
      we_locked            = l_locked
*     WE_LOCK_ORDER        =
*     WE_LOCK_ORDER_USER   =
*     WE_LOCK_TASK         =
*     WE_LOCK_TASK_USER    =
*     WE_OBJECT_EDITABLE   =
*     WE_POSSIBLE_USER_EDIT_TASK       =
    TABLES
      wt_tlock             = lt_locks
    EXCEPTIONS
      empty_key            = 1
      no_systemname        = 2
      no_systemtype        = 3
      unallowed_lock_order = 4.

  p_lock = l_locked.

ENDFORM.                    " F_CHECAR_REQUEST_OBJ

*&---------------------------------------------------------------------*
*&      Form  F_NOVO_NOME_SSF
*&---------------------------------------------------------------------*
FORM f_novo_nome_ssf  USING    p_type_obj
                               p_old_name
                      CHANGING p_new_name.

  FIELD-SYMBOLS: <fs_length>      TYPE any.

  DATA: l_new    TYPE string,
        l_old    TYPE string,
        l_length TYPE i,
        l_field  TYPE string.

  CLEAR: p_new_name.

  CONCATENATE 'C_MAX_LENGTH-' p_type_obj INTO l_field.

  ASSIGN (l_field) TO <fs_length>.

  CHECK <fs_length> IS ASSIGNED.

  l_new = p_old_name.

  CONCATENATE p_preold '_' INTO l_old.

  "[REVISAR] Verificar tipo do objeto?
  REPLACE ALL OCCURRENCES OF    l_old
                          IN    l_new
                          WITH  p_prenew.

  "[REVISAR] Verificar tipo do objeto?
  REPLACE ALL OCCURRENCES OF    p_preold
                          IN    l_new
                          WITH  p_prenew.

  l_length = strlen( l_new ).

  IF l_length <= <fs_length>.
    p_new_name = l_new.
  ENDIF.

ENDFORM.                    " F_NOVO_NOME_SSF

*&---------------------------------------------------------------------*
*&      Form  F_SUBSTITUIR_INCLUDES_FUNCOES
*&---------------------------------------------------------------------*
FORM f_substituir_includes_funcoes .

  DATA:  lw_obj         TYPE y_fugr,
         l_sm30         TYPE c,
         l_programa     TYPE sy-repid,
         lt_includes    TYPE STANDARD TABLE OF char255,
         lw_include     TYPE char255,
         l_regex_find   TYPE string,
         l_regex_change TYPE string,
         lt_rssource    TYPE yt_string.

  CHECK p_subinc IS NOT INITIAL.

  CONCATENATE '(' p_prenew ')(.*)' INTO l_regex_find.
  l_regex_change          = '$1SAPL$2'.

  READ TABLE gt_tadir TRANSPORTING NO FIELDS WITH KEY object =
  c_grupo_funcao BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT gt_tadir INTO gw_tadir FROM sy-tabix.

    IF gw_tadir-object <> c_grupo_funcao.
      EXIT.
    ENDIF.

    READ TABLE gt_fugr INTO lw_obj WITH KEY old_name = gw_tadir-obj_name BINARY SEARCH.

    CHECK sy-subrc = 0.

    PERFORM f_grupo_funcao_sm30 USING lw_obj-old_name CHANGING l_sm30.

    IF l_sm30 IS NOT INITIAL.
      CONTINUE.
    ENDIF.


    l_programa = lw_obj-new_name.

    REPLACE ALL OCCURRENCES OF REGEX l_regex_find
                            IN l_programa
                            WITH l_regex_change
                            IGNORING CASE.

    IF sy-subrc <> 0.
      PERFORM f_add_msg USING c_msgid 'E' '802' text-t04 c_grupo_funcao
       lw_obj-new_name 'Includes' '1'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program             = l_programa
        with_inactive_incls = 'X'
*       WITH_RESERVED_INCLUDES       =
      TABLES
        includetab          = lt_includes
      EXCEPTIONS
        not_existent        = 1
        no_program          = 2.

    CHECK sy-subrc = 0.

    LOOP AT lt_includes INTO lw_include.

      CLEAR: lt_rssource[].

      FIND REGEX l_regex_find
                            IN lw_include
                            IGNORING CASE.

      CHECK sy-subrc = 0.

*      "Normalizar o código do include para 72 posições por linha
*      PERFORM f_rs_source_split_to_72 USING lw_include.
*
*      CHECK sy-subrc = 0.

      "Lê o conteúdo do include da função
      READ REPORT lw_include INTO lt_rssource.

      CHECK sy-subrc = 0.

      PERFORM f_remover_linhas_include_str USING ' ' CHANGING lt_rssource.

      INSERT REPORT lw_include FROM lt_rssource.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " F_SUBSTITUIR_INCLUDES_FUNCOES

*&---------------------------------------------------------------------*
*&      Form  F_REMOVER_LINHAS_INCLUDE_STR
*&---------------------------------------------------------------------*
FORM f_remover_linhas_include_str  USING p_remove_func CHANGING pt_code   TYPE yt_string.

  DATA: lw_code LIKE LINE OF pt_code,
        l_tabix TYPE sy-tabix.

  "Remove linhas do código fonte
*  LOOP AT pt_code INTO lw_code.
*
*    l_tabix = sy-tabix.
*
*    IF p_remove_func IS NOT INITIAL.
*      IF lw_code(8) = 'FUNCTION' OR lw_code CS 'ENDFUNCTION'.
*        DELETE pt_code.
*        CONTINUE.
*      ELSEIF lw_code(2) = '*"'.
*        DELETE pt_code.
*        CONTINUE.
*      ENDIF.
*    ENDIF.

  REPLACE ALL OCCURRENCES OF REGEX gv_regex_module
                          IN TABLE pt_code
                          WITH gv_regex_change IGNORING CASE.

*    IF sy-subrc <> 0.
  REPLACE ALL OCCURRENCES OF REGEX gv_regex_find
                          IN TABLE pt_code
                          WITH gv_regex_change IGNORING CASE.
*    ENDIF.

*    MODIFY pt_code FROM lw_code INDEX l_tabix.

*  ENDLOOP.

ENDFORM.                    " F_REMOVER_LINHAS_INCLUDE_STR
*----------------------------------------------------------------------*
* > [BEGIN] MMSL - 9192 -  14.08.2017 17:30:19
*----------------------------------------------------------------------*
FORM insert_devclass USING  p_devclass
                            p_tadir_old TYPE tadir
                            p_obj_name.

  DATA: lv_obj_name TYPE  tadir-obj_name.

  lv_obj_name = p_obj_name.

  CALL FUNCTION 'PUT_TRINT_TADIR_INSERT'
    EXPORTING
      devclass             = p_devclass
      object               = p_tadir_old-object
      obj_name             = lv_obj_name
      pgmid                = p_tadir_old-pgmid
    EXCEPTIONS
      object_exists_global = 1
      object_exists_local  = 2
      no_authorization     = 3
      OTHERS               = 4.




ENDFORM.
