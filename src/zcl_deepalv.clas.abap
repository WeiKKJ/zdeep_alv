*class ZDEEP_ALV definition
*  public
*  final
*  create public .
*
*public section.
*protected section.
*private section.
*ENDCLASS.
*
*
*
*CLASS ZDEEP_ALV IMPLEMENTATION.
*ENDCLASS.
class ZCL_DEEPALV definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods DISPLAY_DEEP_STRUCTURE_COMPDES
    importing
      value(I_DEEPSTRC) type DATA
      value(I_CALLBACK_USER_COMMAND) type SLIS_FORMNAME optional
      value(I_CALLBACK_PF_STATUS_SET) type SLIS_FORMNAME optional
      value(I_CUSTOM_FCAT) type LVC_T_FCAT optional
      value(I_CUSTOM_LAYO) type LVC_S_LAYO optional
    exceptions
      TYPE_ERROR .
  methods HOTSPOT_CLICK_COMPDESCR
    importing
      value(I_SELFIELD) type SLIS_SELFIELD .
  methods BACK_CLICK .
  PROTECTED SECTION.
private section.

  data MV_CALLBACK_PF_STATUS_SET type SLIS_FORMNAME .
  data MV_CALLBACK_USER_COMMAND type SLIS_FORMNAME .
  data:
    mo_display_stack TYPE STANDARD TABLE OF REF TO data .
  constants MC_STACK_GO type CHAR1 value '1' ##NO_TEXT.
  constants MC_STACK_BACK type CHAR1 value '2' ##NO_TEXT.
  data MT_COMPONENTS type ABAP_COMPONENT_TAB .
  data MT_CUSTOM_FCAT type LVC_T_FCAT .
  data MT_COMPDESCR type ABAP_COMPDESCR_TAB .

  methods _UPDATE_DISPLAY_STACK
    importing
      !TABLE type ANY TABLE optional
      !OPERATION type CHAR1
    exceptions
      NO_DATA .
  methods _GET_TYPE_HANDLE_COMPDESCR
    importing
      !TABLE type ANY TABLE
    returning
      value(NEW_TABLE) type ref to DATA .
  methods _FILL_CONDENSED_DATA_COMPDESCR
    importing
      value(TABLE) type ANY TABLE
    changing
      !CONDENSED_DATA type ANY TABLE .
  methods _DISPLAY_CONDENSED_ALV
    importing
      value(CONDENSED_DATA) type ANY TABLE
      !PF_STATUS_SET type SLIS_FORMNAME optional
      !USER_COMMAND type SLIS_FORMNAME optional
      !CUSTOM_FIELDCAT_LVC type LVC_T_FCAT optional
    changing
      !FIELDCAT_LVC type LVC_T_FCAT optional
      !LAYOUT_LVC type LVC_S_LAYO optional .
  methods _BUILD_FIELDCAT
    importing
      !TABLE type ANY TABLE
    returning
      value(FIELDCAT) type LVC_T_FCAT .
  methods _BUILD_FIELDCAT_COMPDESCR
    importing
      !TABLE type ANY TABLE
    returning
      value(FIELDCAT) type LVC_T_FCAT .
  methods _GET_LINE_COMP
    importing
      value(TABLE) type ANY TABLE
    exporting
      value(COMPONENT_TABLE) type ABAP_COMPONENT_TAB
      value(COMPDESCR_TABLE) type ABAP_COMPDESCR_TAB .
ENDCLASS.



CLASS ZCL_DEEPALV IMPLEMENTATION.


  METHOD back_click.
    _update_display_stack( operation = mc_stack_back ).
  ENDMETHOD.


  METHOD constructor.
  ENDMETHOD.


  METHOD display_deep_structure_compdes.
    DATA:lt_fieldcat TYPE lvc_t_fcat,
         comp        TYPE abap_component_tab,
         msg         TYPE string.
    DATA:dref_tab   TYPE REF TO data,
         ref_line   TYPE REF TO data,
         data_type  TYPE REF TO cl_abap_datadescr,
         table_type TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS:<dref_tab>    TYPE STANDARD TABLE,
                  <tab>         TYPE STANDARD TABLE,
                  <fs_new_data> TYPE STANDARD TABLE.
*将I_DEEPSTRC始终构造为内表
    IF i_deepstrc IS INITIAL.
      MESSAGE '无数据!' TYPE 'S'.
      RETURN.
    ENDIF.
    data_type ?= cl_abap_datadescr=>describe_by_data( i_deepstrc ).
    CASE data_type->type_kind.
      WHEN 'u' OR 'v'.
        table_type = cl_abap_tabledescr=>create( data_type ).
        CREATE DATA dref_tab TYPE HANDLE table_type.
        ASSIGN dref_tab->* TO <dref_tab>.
        INSERT i_deepstrc INTO TABLE <dref_tab>.
      WHEN 'h'.
        table_type ?= data_type.
        DATA(line_type) = table_type->get_table_line_type( ).
        CASE line_type->kind.
          WHEN 'S'.
*& kkw 添加对排序内表的展示支持
            IF table_type->table_kind NE table_type->tablekind_std.
              CREATE DATA ref_line TYPE HANDLE line_type.
              ASSIGN ref_line->* TO FIELD-SYMBOL(<ref_line>).
              CREATE DATA dref_tab LIKE STANDARD TABLE OF <ref_line>.
              ASSIGN dref_tab->* TO <dref_tab>.
            ELSE.
              CREATE DATA dref_tab TYPE HANDLE data_type.
              ASSIGN dref_tab->* TO <dref_tab>.
            ENDIF.
*& End  04.11.2024 10:36:58
            <dref_tab> = i_deepstrc.
          WHEN 'E'.
*& kkw 添加对单元素内表的展示支持
            CLEAR comp.
            DATA(elem_type) = CAST cl_abap_elemdescr( line_type ).
            INSERT INITIAL LINE INTO TABLE comp ASSIGNING FIELD-SYMBOL(<comp>).
            <comp>-name         = elem_type->get_relative_name( ).
            <comp>-type         ?= elem_type.
            <comp>-as_include   = ''.
            <comp>-suffix       = ''.
            DATA(struc) = cl_abap_structdescr=>create( comp ).
            DATA(dt) = cl_abap_tabledescr=>create( p_line_type = struc ).
            CREATE DATA dref_tab TYPE HANDLE dt.
            ASSIGN dref_tab->* TO <dref_tab>.
            ASSIGN i_deepstrc TO <tab>.
            IF sy-subrc EQ 0.
              LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<tabline>).
                INSERT INITIAL LINE INTO TABLE <dref_tab> ASSIGNING FIELD-SYMBOL(<dref_tabline>).
                ASSIGN COMPONENT elem_type->get_relative_name( ) OF STRUCTURE <dref_tabline> TO FIELD-SYMBOL(<value>).
                <value> = <tabline>.
              ENDLOOP.
              UNASSIGN <tab>.
            ENDIF.
          WHEN OTHERS.
            msg = |不受支持的数据类型【{ line_type->kind }】:【{ line_type->get_relative_name( ) }】|.
            MESSAGE msg TYPE 'E'.
        ENDCASE.
*& End
      WHEN OTHERS.
        msg = |不受支持的数据类型:【{ data_type->kind }】:【{ data_type->get_relative_name( ) }】|.
        MESSAGE msg TYPE 'E'.
    ENDCASE.
    CHECK <dref_tab> IS ASSIGNED.
*   将展示数据使用堆栈管理
    _update_display_stack( table = <dref_tab> operation = mc_stack_go ).
*   绑定全局变量
    IF i_callback_pf_status_set IS NOT INITIAL.
      mv_callback_pf_status_set = i_callback_pf_status_set.
    ENDIF.
    IF i_callback_user_command IS NOT INITIAL.
      mv_callback_user_command = i_callback_user_command.
    ENDIF.
    IF i_custom_fcat IS NOT INITIAL.
      mt_custom_fcat = i_custom_fcat.
    ENDIF.
    CLEAR:mt_compdescr,mt_components.

    CALL METHOD me->_get_line_comp
      EXPORTING
        table           = <dref_tab>
      IMPORTING
        component_table = mt_components
        compdescr_table = mt_compdescr.
*   判断是否包含表类型或者结构类型字段
    LOOP AT mt_compdescr TRANSPORTING NO FIELDS
      WHERE ( type_kind = cl_abap_elemdescr=>typekind_table
      OR type_kind = cl_abap_elemdescr=>typekind_struct2
      OR type_kind = cl_abap_elemdescr=>typekind_struct1
      )
      .
      EXIT.
    ENDLOOP.
*   如果存在表类型字段
    IF sy-subrc = 0.
*     构建新的扁平类型的内表
      DATA(lo_new_data_ref) = _get_type_handle_compdescr( <dref_tab> ).
      ASSIGN lo_new_data_ref->* TO <fs_new_data>.
*     填充新的扁平类型的内表
      _fill_condensed_data_compdescr( EXPORTING table = <dref_tab> CHANGING  condensed_data = <fs_new_data> ).
    ELSE.
      ASSIGN <dref_tab> TO <fs_new_data>.
    ENDIF.
*   构建fieldcat内表
    lt_fieldcat = _build_fieldcat_compdescr( table = <dref_tab> ).
*   展示ALV
    me->_display_condensed_alv(
    EXPORTING
      condensed_data      = <fs_new_data>
      pf_status_set       = mv_callback_pf_status_set
      user_command        = mv_callback_user_command
      custom_fieldcat_lvc = mt_custom_fcat
    CHANGING
      fieldcat_lvc        = lt_fieldcat
      layout_lvc          = i_custom_layo
      ).
  ENDMETHOD.


  METHOD hotspot_click_compdescr.
    FIELD-SYMBOLS:
      <fs_current_display> TYPE INDEX TABLE,
      <fs_table>           TYPE ANY TABLE,
      <fs_struc>           TYPE any.
    DATA:lo_current_display TYPE REF TO data,
         lv_obj_type        TYPE char1,
         lo_data            TYPE REF TO data.

*   获取当前界面展示扁平内表对应的原始内表（深层内表）
    READ TABLE mo_display_stack INTO lo_current_display INDEX 1.
    ASSIGN lo_current_display->* TO <fs_current_display>.

    READ TABLE <fs_current_display> INDEX i_selfield-tabindex ASSIGNING FIELD-SYMBOL(<fs_current_row>).
    IF <fs_current_row> IS ASSIGNED.
*     获取点击单元格数据内容
      ASSIGN COMPONENT i_selfield-fieldname OF STRUCTURE <fs_current_row> TO FIELD-SYMBOL(<fs_deep_field>).
*     获取点击单元格数据类型
      lv_obj_type = cl_abap_tabledescr=>describe_by_data( <fs_deep_field> )->type_kind.
      CASE lv_obj_type.
*       表
        WHEN cl_abap_elemdescr=>typekind_table.
          ASSIGN <fs_deep_field> TO <fs_table>.
*       深层结构或者扁平结构
        WHEN cl_abap_elemdescr=>typekind_struct2 OR cl_abap_elemdescr=>typekind_struct1.
          CREATE DATA lo_data LIKE TABLE OF <fs_deep_field>.
          ASSIGN lo_data->* TO <fs_table>.
          INSERT INITIAL LINE INTO TABLE <fs_table> ASSIGNING FIELD-SYMBOL(<line>).
          <line> = <fs_deep_field> .
        WHEN OTHERS.
      ENDCASE.
      IF <fs_table> IS ASSIGNED.
        IF <fs_table> IS NOT INITIAL.
          display_deep_structure_compdes( i_deepstrc = <fs_table> i_callback_user_command = mv_callback_user_command  ).
        ELSE.
          MESSAGE '无数据!' TYPE 'S'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD _build_fieldcat.
    DATA:
      lv_struc_name TYPE tabname,
      ls_fieldcat   TYPE lvc_s_fcat.

*   获取行类型结构名
    DATA(lv_structure) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data( table ) )->get_table_line_type( )->get_relative_name( ).

    lv_struc_name = lv_structure.

*   获取fieldcat信息
    SELECT
      a~tabname,
      a~fieldname,
      a~position,
      a~rollname,
      a~inttype,
      a~datatype,
      a~leng,
      a~domname,
      a~comptype,
      b~reftable,
      b~reffield,
      b~reptext,
      b~scrtext_s,
      b~scrtext_m,
      b~scrtext_l,
      c~lowercase,
      c~convexit,
      d~ddtext AS d_ddtext,
      e~ddtext AS t_ddtext,
      f~ddtext AS s_ddtext
      FROM dd03l AS a
      LEFT OUTER JOIN dd03vt AS b
      ON a~tabname    = b~tabname
      AND a~fieldname  = b~fieldname
      AND a~position   = b~position
      AND a~comptype   = b~comptype
      AND b~ddlanguage = @sy-langu
      LEFT OUTER JOIN dd04l AS c
      ON a~rollname   = c~rollname
      AND a~as4local   = c~as4local
      LEFT OUTER JOIN dd03t AS d
      ON a~tabname    = d~tabname
      AND a~as4local   = d~as4local
      AND a~fieldname  = d~fieldname
      AND d~ddlanguage = @sy-langu
      LEFT OUTER JOIN dd40t AS e
      ON a~rollname   = e~typename
      AND e~ddlanguage = @sy-langu
      LEFT OUTER JOIN dd02t AS f
      ON a~rollname = f~tabname
      AND f~ddlanguage = @sy-langu
      WHERE a~tabname    = @lv_struc_name
      AND a~as4local   = 'A'
      AND a~depth      = ''
*      AND a~comptype NE 'S'
      ORDER BY a~position
      INTO TABLE @DATA(lt_fieldinfo).

    LOOP AT lt_fieldinfo INTO DATA(ls_fieldinfo).
      ls_fieldcat-col_pos   = ls_fieldinfo-position.
      ls_fieldcat-fieldname = ls_fieldinfo-fieldname.

      CASE ls_fieldinfo-datatype.
*       数量类型
        WHEN 'QUAN'.
          ls_fieldcat-qfieldname = ls_fieldinfo-reffield.
*       金额类型
        WHEN 'CURR'.
          ls_fieldcat-cfieldname = ls_fieldinfo-reffield.
        WHEN OTHERS.
      ENDCASE.

      ls_fieldcat-convexit  = ls_fieldinfo-convexit.
      ls_fieldcat-datatype  = ls_fieldinfo-datatype.
      ls_fieldcat-inttype   = ls_fieldinfo-inttype.
      ls_fieldcat-intlen    = ls_fieldinfo-leng.
      ls_fieldcat-lowercase = ls_fieldinfo-lowercase.
      ls_fieldcat-reptext   =  ls_fieldinfo-fieldname && '(' && ls_fieldinfo-reptext && ')'.

      CASE ls_fieldinfo-comptype.
*       内置类型
        WHEN ''.
          ls_fieldcat-reptext = ls_fieldinfo-fieldname && '(' && ls_fieldinfo-d_ddtext && ')'.
*       表类型
        WHEN 'L'.
          ls_fieldcat-hotspot = 'X'.
          ls_fieldcat-reptext = ls_fieldinfo-fieldname && '(' && ls_fieldinfo-t_ddtext && ')'.
          ls_fieldcat-just    = 'L'.
*       结构
        WHEN 'S'.
          ls_fieldcat-hotspot = 'X'.
          ls_fieldcat-reptext = ls_fieldinfo-fieldname && '(' && ls_fieldinfo-s_ddtext && ')'.
          ls_fieldcat-just    = 'L'.
        WHEN OTHERS.
      ENDCASE.

      ls_fieldcat-domname   = ls_fieldinfo-domname.
      ls_fieldcat-ref_table = lv_struc_name.
      ls_fieldcat-dd_outlen = ls_fieldinfo-leng.
*      ls_fieldcat-scrtext_s = ls_fieldinfo-scrtext_s.
*      ls_fieldcat-scrtext_m = ls_fieldinfo-scrtext_m.
*      ls_fieldcat-scrtext_l = ls_fieldinfo-scrtext_l.
      ls_fieldcat-scrtext_s = ls_fieldcat-reptext.
      ls_fieldcat-scrtext_m = ls_fieldcat-reptext.
      ls_fieldcat-scrtext_l = ls_fieldcat-reptext.
      APPEND ls_fieldcat TO fieldcat.
      CLEAR ls_fieldcat.
    ENDLOOP.
  ENDMETHOD.


  METHOD _build_fieldcat_compdescr.
    DATA:lv_struc_name TYPE tabname,
         ls_fieldcat   TYPE lvc_s_fcat,
         position      TYPE dd03l-position.
*   获取行类型结构名
    DATA(lv_structure) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data( table ) )->get_table_line_type( )->get_relative_name( ).
    lv_struc_name = lv_structure.
    TRY.
        DATA(compdescr_tab) = CAST cl_abap_structdescr(
        CAST cl_abap_tabledescr(
        cl_abap_tabledescr=>describe_by_data( table )
        )->get_table_line_type( )
        )->components.
        CLEAR:position.
        LOOP AT compdescr_tab INTO DATA(ls_fieldinfo).
          ADD 1 TO position.
          ls_fieldcat-col_pos   = position.
          ls_fieldcat-fieldname = ls_fieldinfo-name.

*      CASE ls_fieldinfo-datatype.
**       数量类型
*        WHEN 'QUAN'.
*          ls_fieldcat-qfieldname = ls_fieldinfo-reffield.
**       金额类型
*        WHEN 'CURR'.
*          ls_fieldcat-cfieldname = ls_fieldinfo-reffield.
*        WHEN OTHERS.
*      ENDCASE.

*      ls_fieldcat-convexit  = ls_fieldinfo-convexit.
*      ls_fieldcat-datatype  = ls_fieldinfo-datatype.
*      ls_fieldcat-inttype   = ls_fieldinfo-inttype.
          ls_fieldcat-intlen    = ls_fieldinfo-length.
*      ls_fieldcat-lowercase = ls_fieldinfo-lowercase.
          ls_fieldcat-reptext   =  ls_fieldinfo-name.

          CASE ls_fieldinfo-type_kind.
*       表
            WHEN 'h'.
              ls_fieldcat-hotspot = 'X'.
              ls_fieldcat-reptext = ls_fieldinfo-name.
              ls_fieldcat-just    = 'L'.
*       纵深结构
            WHEN 'v'.
              ls_fieldcat-hotspot = 'X'.
              ls_fieldcat-reptext = ls_fieldinfo-name.
              ls_fieldcat-just    = 'L'.
*       扁平结构
            WHEN 'u'.
              ls_fieldcat-hotspot = 'X'.
              ls_fieldcat-reptext = ls_fieldinfo-name.
              ls_fieldcat-just    = 'L'.
            WHEN OTHERS.
          ENDCASE.

*      ls_fieldcat-domname   = ls_fieldinfo-domname.
          ls_fieldcat-ref_table = lv_struc_name.
          ls_fieldcat-dd_outlen = ls_fieldinfo-length.
          ls_fieldcat-scrtext_s = ls_fieldcat-reptext.
          ls_fieldcat-scrtext_m = ls_fieldcat-reptext.
          ls_fieldcat-scrtext_l = ls_fieldcat-reptext.
          APPEND ls_fieldcat TO fieldcat.
          CLEAR ls_fieldcat.
        ENDLOOP.
      CATCH cx_root.
*        DATA(elem_type) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data( table ) )->get_table_line_type( ).
*        INSERT INITIAL LINE INTO TABLE fieldcat ASSIGNING FIELD-SYMBOL(<fieldcat>).
*        <fieldcat>-col_pos   = 1.
*        <fieldcat>-fieldname = 'TABLINE'.
*        <fieldcat>-intlen    = elem_type->length.
**        <fieldcat>-reptext   = <fieldcat>-name.
**        <fieldcat>-ref_table = lv_struc_name.
*        <fieldcat>-dd_outlen = elem_type->length.
*        <fieldcat>-scrtext_s = <fieldcat>-reptext.
*        <fieldcat>-scrtext_m = <fieldcat>-reptext.
*        <fieldcat>-scrtext_l = <fieldcat>-reptext.
    ENDTRY.
  ENDMETHOD.


  METHOD _display_condensed_alv.
    DATA:
      lv_tabix TYPE i,
      lv_index TYPE i.

    DATA:
      lt_event_exit TYPE slis_t_event_exit,
      ls_event_exit TYPE slis_event_exit.

    FIELD-SYMBOLS <fs_alv> TYPE STANDARD TABLE.

*   设置默认布局控制
    IF layout_lvc IS INITIAL.
      layout_lvc-zebra      = 'X'.                "颜色间隔
      layout_lvc-sel_mode   = 'D'.                "选择模式
      layout_lvc-cwidth_opt = 'A'.                "列宽自适应
    ENDIF.
*   如果存在自定义字段设置，将其与标准获取的字段设置合并
    IF custom_fieldcat_lvc IS NOT INITIAL.
      LOOP AT custom_fieldcat_lvc INTO DATA(ls_cus_fcat).
        READ TABLE fieldcat_lvc ASSIGNING FIELD-SYMBOL(<fs_fieldcat>)
        WITH KEY fieldname = ls_cus_fcat-fieldname
        ref_table = ls_cus_fcat-ref_table.
        IF sy-subrc = 0.
          DO.
            ADD 1 TO lv_index.
            ASSIGN COMPONENT lv_index OF STRUCTURE ls_cus_fcat TO FIELD-SYMBOL(<fs_cus_fcat>).
            IF <fs_cus_fcat> IS ASSIGNED.
              ASSIGN COMPONENT lv_index OF STRUCTURE <fs_fieldcat> TO FIELD-SYMBOL(<fs_fcat>).
              IF <fs_cus_fcat> <> <fs_fcat> AND <fs_cus_fcat> IS NOT INITIAL.
                <fs_fcat> = <fs_cus_fcat>.
              ENDIF.
            ELSE.
              EXIT.
            ENDIF.
            UNASSIGN:
            <fs_cus_fcat>,
            <fs_fcat>.
          ENDDO.
          CLEAR lv_index.
        ENDIF.
      ENDLOOP.
    ENDIF.
    ASSIGN condensed_data TO <fs_alv>.
*   使标准按钮->"返回"，可以触发用户自定义USER_COMMAND回调函数
    ls_event_exit-ucomm = '&F03'.
    ls_event_exit-after = 'X'.
    APPEND ls_event_exit TO lt_event_exit.
*   展示ALV
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program       = sy-cprog
        i_callback_pf_status_set = pf_status_set
        i_callback_user_command  = user_command
        is_layout_lvc            = layout_lvc
        it_fieldcat_lvc          = fieldcat_lvc
        i_save                   = 'A'
        it_event_exit            = lt_event_exit
      TABLES
        t_outtab                 = <fs_alv>
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
  ENDMETHOD.


  METHOD _fill_condensed_data_compdescr.
    FIELD-SYMBOLS:<fs_table> TYPE ANY TABLE.
    LOOP AT table ASSIGNING FIELD-SYMBOL(<fs_line>).
      INSERT INITIAL LINE INTO TABLE condensed_data ASSIGNING FIELD-SYMBOL(<fs_new_line>).
      LOOP AT mt_compdescr ASSIGNING FIELD-SYMBOL(<fs_compdescr>).
        CASE <fs_compdescr>-type_kind.
*         表类型
          WHEN cl_abap_elemdescr=>typekind_table.
            ASSIGN COMPONENT <fs_compdescr>-name OF STRUCTURE <fs_line> TO <fs_table>.
            IF <fs_table> IS ASSIGNED.
              ASSIGN COMPONENT <fs_compdescr>-name OF STRUCTURE <fs_new_line> TO FIELD-SYMBOL(<fs_tab_desc>).
              IF <fs_tab_desc> IS ASSIGNED.
                <fs_tab_desc> = |{ icon_list }{ <fs_compdescr>-name }[ { lines( <fs_table> ) } ]|.
                UNASSIGN:
                <fs_tab_desc>,
                <fs_table>.
              ENDIF.
            ENDIF.
*         结构类型 或 扁平结构
          WHEN cl_abap_elemdescr=>typekind_struct2 OR cl_abap_elemdescr=>typekind_struct1.
            ASSIGN COMPONENT <fs_compdescr>-name OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_struc>).
            IF <fs_struc> IS ASSIGNED.
              ASSIGN COMPONENT <fs_compdescr>-name OF STRUCTURE <fs_new_line> TO <fs_tab_desc>.
              IF <fs_tab_desc> IS ASSIGNED.
                <fs_tab_desc> = |{ icon_structure }{ <fs_compdescr>-name }|.
                UNASSIGN:
                <fs_tab_desc>,
                <fs_struc>.
              ENDIF.
            ENDIF.
*         其他类型
          WHEN OTHERS.
            ASSIGN COMPONENT <fs_compdescr>-name OF STRUCTURE <fs_line> TO  FIELD-SYMBOL(<fs_value>).
            IF <fs_value> IS ASSIGNED.
              ASSIGN COMPONENT <fs_compdescr>-name OF STRUCTURE <fs_new_line> TO  FIELD-SYMBOL(<fs_value_new>).
              IF <fs_value_new> IS ASSIGNED.
                <fs_value_new> = <fs_value>.
                UNASSIGN:
                <fs_value>,
                <fs_value_new>.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD _get_type_handle_compdescr.
*   创建新的扁平类型对象，将表类型或者深层结构字段替换为CHAR类型
    DATA:componentdescr     TYPE abap_componentdescr,
         componentdescr_tab TYPE abap_component_tab.

    LOOP AT mt_compdescr ASSIGNING FIELD-SYMBOL(<mt_compdescr>).
      CLEAR:componentdescr.
      componentdescr-name = <mt_compdescr>-name.
      CASE <mt_compdescr>-type_kind.
        WHEN cl_abap_elemdescr=>typekind_table.
          READ TABLE mt_components INTO DATA(wa_mt_components) WITH KEY name = <mt_compdescr>-name.
          IF sy-subrc EQ 0.
            componentdescr-type ?= cl_abap_elemdescr=>get_c( p_length = 70 ).
          ELSE.
            CONTINUE.
          ENDIF.
        WHEN cl_abap_elemdescr=>typekind_struct2 OR cl_abap_elemdescr=>typekind_struct1.
          componentdescr-type ?= cl_abap_elemdescr=>get_c( p_length = 70 ).
        WHEN OTHERS.
          componentdescr-type ?= cl_abap_elemdescr=>get_by_kind( p_type_kind = <mt_compdescr>-type_kind p_length = <mt_compdescr>-length p_decimals = <mt_compdescr>-decimals ).
      ENDCASE.
      APPEND componentdescr TO componentdescr_tab.
    ENDLOOP.

    DATA(lo_new_tab) = cl_abap_tabledescr=>create(
          p_line_type = cl_abap_structdescr=>create( componentdescr_tab )
          p_table_kind = cl_abap_tabledescr=>tablekind_std
          p_unique     = abap_false
          ).
    CREATE DATA new_table TYPE HANDLE lo_new_tab.
  ENDMETHOD.


  METHOD _update_display_stack.
    DATA:lo_curr_disp TYPE REF TO data.
    FIELD-SYMBOLS:<table> TYPE ANY TABLE.
    CASE operation.
      WHEN mc_stack_back.
        DELETE mo_display_stack INDEX 1.
      WHEN mc_stack_go.
        IF table IS NOT SUPPLIED.
          RAISE no_data.
        ELSE.
*         获取类型对象
          DATA(lo_handle) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data( table ) ).
*         基于类型对象创建数据
          CREATE DATA lo_curr_disp TYPE HANDLE lo_handle.
*         指针分配
          ASSIGN lo_curr_disp->* TO <table>.
*          LOOP AT table ASSIGNING FIELD-SYMBOL(<line>).
*            INSERT INITIAL LINE INTO TABLE <table> ASSIGNING FIELD-SYMBOL(<new_line>).
*            <new_line> = CORRESPONDING #( <line> ).
*          ENDLOOP.
          <table> = table.
*         将数据保存至堆栈最上层
          INSERT lo_curr_disp INTO mo_display_stack INDEX 1.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD _get_line_comp.
    TRY.
        DATA(struct_type) = CAST cl_abap_structdescr(
        CAST cl_abap_tabledescr(
        cl_abap_tabledescr=>describe_by_data( table )
        )->get_table_line_type( )
        ).
      CATCH cx_root INTO DATA(exc).
        DATA(errtext) = exc->get_text( ).
        MESSAGE errtext TYPE 'E'.
    ENDTRY.
    component_table = struct_type->get_components( ).
    compdescr_table = struct_type->components.
  ENDMETHOD.
ENDCLASS.
