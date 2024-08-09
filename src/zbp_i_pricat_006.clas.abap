CLASS zbp_i_pricat_006 DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zi_pricat_006.

*   Get Description for Article, Color, Pricat, Series, DTB Group (via Custom Business Object ODATA API)
    CLASS-METHODS get_custom_fields_internal
        IMPORTING VALUE(i_article_code)         TYPE string OPTIONAL
                  VALUE(i_color_code)           TYPE string OPTIONAL
                  VALUE(i_pricat_code)          TYPE string OPTIONAL
                  VALUE(i_series_code)          TYPE string OPTIONAL
                  VALUE(i_dtbgroup_code)        TYPE string OPTIONAL
        EXPORTING VALUE(o_article_description)  TYPE string
                  VALUE(o_color_description)    TYPE string
                  VALUE(o_pricat_description)   TYPE string
                  VALUE(o_series_description)   TYPE string
                  VALUE(o_dtbgroup_description) TYPE string
                  VALUE(o_article_code)         TYPE string
                  VALUE(o_color_code)           TYPE string
                  VALUE(o_pricat_code)          TYPE string
                  VALUE(o_series_code)          TYPE string
                  VALUE(o_dtbgroup_code)        TYPE string.

    TYPES: BEGIN OF descr_table. " for cashing
        TYPES code(4)          TYPE C.
        TYPES description(40)  TYPE C.
    TYPES: END OF descr_table.
    CLASS-DATA it_article   TYPE TABLE OF descr_table. " WITH UNIQUE KEY code.
    CLASS-DATA it_color     TYPE TABLE OF descr_table. " WITH UNIQUE KEY code.
    CLASS-DATA it_pricat    TYPE TABLE OF descr_table. " WITH UNIQUE KEY code.
    CLASS-DATA it_series    TYPE TABLE OF descr_table. " WITH UNIQUE KEY code.
    CLASS-DATA it_dtbgroup  TYPE TABLE OF descr_table. " WITH UNIQUE KEY code.

*   Get Description for Article, Color, Pricat, Series, DTB Group (via Custom Business Object ODATA API) - optimized via cash
    CLASS-METHODS get_custom_fields_opt_internal
        IMPORTING VALUE(i_article_code)         TYPE string OPTIONAL
                  VALUE(i_color_code)           TYPE string OPTIONAL
                  VALUE(i_pricat_code)          TYPE string OPTIONAL
                  VALUE(i_series_code)          TYPE string OPTIONAL
                  VALUE(i_dtbgroup_code)        TYPE string OPTIONAL
        EXPORTING VALUE(o_article_description)  TYPE string
                  VALUE(o_color_description)    TYPE string
                  VALUE(o_pricat_description)   TYPE string
                  VALUE(o_series_description)   TYPE string
                  VALUE(o_dtbgroup_description) TYPE string
                  VALUE(o_article_code)         TYPE string
                  VALUE(o_color_code)           TYPE string
                  VALUE(o_pricat_code)          TYPE string
                  VALUE(o_series_code)          TYPE string
                  VALUE(o_dtbgroup_code)        TYPE string.

    CLASS-DATA skip_rows_filling TYPE C. " Skip product rows filling

*   Enrich Product Row With Product Data
    CLASS-METHODS enrich_product_row_internal
        IMPORTING VALUE(i_product)  TYPE i_product
        EXPORTING VALUE(o_product)  TYPE zc_product_006.

ENDCLASS. " zbp_i_pricat_006 DEFINITION

CLASS zbp_i_pricat_006 IMPLEMENTATION.

  METHOD get_custom_fields_internal. " Get Description for Article, Color, Pricat, Series, DTB Group (via Custom Business Object ODATA API)

    DATA system_url TYPE string.

    DATA i_username TYPE string VALUE 'INBOUND_USER'.
    DATA i_password TYPE string VALUE 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.

    DATA token      TYPE string.
    DATA body       TYPE string.
    DATA text       TYPE string.
    DATA s1         TYPE string.
    DATA s2         TYPE string.
    DATA s3         TYPE string.

    TRY.

*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS(guid'91bf6b38-1c0f-1ede-b2ca-79c4ed0310fc')'.
*  DATA(i_url) = 'https://my404898.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'.

        system_url = cl_abap_context_info=>get_system_url( ).


*       Read list of objects and get UUID of the first one:

        CONCATENATE
                'https://'
                system_url(8) " my404898
                '-api.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'
            INTO DATA(i_url).

        IF ( system_url(8) = 'my404907' ). " test
            i_username  = 'INBOUND_USER'.
            i_password  = 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.
        ENDIF.
        IF ( system_url(8) = 'my410080' ). " prod
            i_username  = 'INBOUND_USER'.
            i_password  = 'YKXMYdjNnGgqko&aEueVx5mHTFPRGcDGAVgQgnFh'.
        ENDIF.

        DATA(http_destination) = cl_http_destination_provider=>create_by_url( i_url = i_url ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( http_destination ).

        lo_http_client->get_http_request( )->set_authorization_basic(
            i_username = i_username
            i_password = i_password
        ).

        DATA(lo_http_request) = lo_http_client->get_http_request( ).

        DATA(lo_http_response) = lo_http_client->execute(
            i_method   = if_web_http_client=>get
        ).

        text                          = lo_http_response->get_text( ).
        DATA(status)                  = lo_http_response->get_status( ).
        DATA(response_header_fields)  = lo_http_response->get_header_fields( ).

        REPLACE '<d:SAP_UUID>'    IN text WITH '***SAP_UUID***'.
        REPLACE '</d:SAP_UUID>'   IN text WITH '***SAP_UUID***'.
        SPLIT text AT '***SAP_UUID***' INTO s1 s2 s3.

        DATA(sap_uuid) = s2.

        CONCATENATE
                'https://'
                system_url(8) " my404898
                '-api.s4hana.cloud.sap/sap/opu/odata/sap/YY1_CUSTOMFIELDS_CDS/YY1_CUSTOMFIELDS'
                '(guid''' sap_uuid ''')' " '(guid''91bf6b38-1c0f-1ede-b2ca-79c4ed0310fc'')'
            INTO i_url.

        http_destination = cl_http_destination_provider=>create_by_url( i_url = i_url ).

        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( http_destination ).

        lo_http_client->get_http_request( )->set_authorization_basic(
            i_username = i_username
            i_password = i_password
        ).

        lo_http_request = lo_http_client->get_http_request( ).


*       Get Token:

        lo_http_request->set_header_field(
            i_name  = 'x-csrf-token'
            i_value = 'fetch'
        ).

        lo_http_response = lo_http_client->execute(
            i_method   = if_web_http_client=>get
        ).

        text                   = lo_http_response->get_text( ).
        status                 = lo_http_response->get_status( ).
        response_header_fields = lo_http_response->get_header_fields( ).

        READ TABLE response_header_fields WITH KEY name = 'x-csrf-token' INTO DATA(field).
        IF ( sy-subrc = 0 ).
            token = field-value.
        ENDIF.


*       Update Codes:

        DATA i_fields TYPE if_web_http_request=>name_value_pairs.
        APPEND VALUE #(
            name  = 'x-csrf-token'
            value = token " '5iGZK1qT45Vi4UfHYazbPQ=='
        )
        TO i_fields.
        APPEND VALUE #(
            name  = 'Content-Type'
            value = 'application/json'
        )
        TO i_fields.
*        APPEND VALUE #(
*            name  = 'Content-Length'
*            value = '1000'
*        )
*        TO i_fields.

        lo_http_request->set_header_fields(
          EXPORTING
            i_fields = i_fields
*          RECEIVING
*            r_value  =
        ).

        CONCATENATE
                '{'
                '"ARTICLE_CODE":"' i_article_code '",'
                '"COLOR_CODE":"' i_color_code '",'
                '"PRICAT_CODE":"' i_pricat_code '",'
                '"SERIES_CODE":"' i_series_code '",'
                '"DTB_CODE":"' i_dtbgroup_code '"'
                '}'
            INTO
                body.

        lo_http_request->set_text(
          EXPORTING
            i_text   = body
*            i_offset = 0
*            i_length = -1
*          RECEIVING
*            r_value  =
        ).

        lo_http_response = lo_http_client->execute(
            i_method   = if_web_http_client=>put
        ).

        text                      = lo_http_response->get_text( ).
        status                    = lo_http_response->get_status( ).
        response_header_fields    = lo_http_response->get_header_fields( ).


*       Read Descriptions

        lo_http_response = lo_http_client->execute(
            i_method   = if_web_http_client=>get
        ).

        text                    = lo_http_response->get_text( ).
        status                  = lo_http_response->get_status( ).
        response_header_fields  = lo_http_response->get_header_fields( ).

        REPLACE '<d:ARTICLE_DESCRIPTION>'   IN text WITH '***ARTICLE_DESCRIPTION***'.
        REPLACE '</d:ARTICLE_DESCRIPTION>'  IN text WITH '***ARTICLE_DESCRIPTION***'.
        SPLIT text AT '***ARTICLE_DESCRIPTION***' INTO s1 s2 s3.
        o_article_description = s2.
        o_article_code        = i_article_code.

        REPLACE '<d:COLOR_DESCRIPTION>'     IN text WITH '***COLOR_DESCRIPTION***'.
        REPLACE '</d:COLOR_DESCRIPTION>'    IN text WITH '***COLOR_DESCRIPTION***'.
        SPLIT text AT '***COLOR_DESCRIPTION***' INTO s1 s2 s3.
        o_color_description = s2.
        o_color_code        = i_color_code.

        REPLACE '<d:PRICAT_DESCRIPTION>'     IN text WITH '***PRICAT_DESCRIPTION***'.
        REPLACE '</d:PRICAT_DESCRIPTION>'    IN text WITH '***PRICAT_DESCRIPTION***'.
        SPLIT text AT '***PRICAT_DESCRIPTION***' INTO s1 s2 s3.
        o_pricat_description = s2.
        o_pricat_code        = i_pricat_code.

        REPLACE '<d:SERIES_DESCRIPTION>'     IN text WITH '***SERIES_DESCRIPTION***'.
        REPLACE '</d:SERIES_DESCRIPTION>'    IN text WITH '***SERIES_DESCRIPTION***'.
        SPLIT text AT '***SERIES_DESCRIPTION***' INTO s1 s2 s3.
        o_series_description = s2.
        o_series_code        = i_series_code.

        REPLACE '<d:DTB_DESCRIPTION>'     IN text WITH '***DTB_DESCRIPTION***'.
        REPLACE '</d:DTB_DESCRIPTION>'    IN text WITH '***DTB_DESCRIPTION***'.
        SPLIT text AT '***DTB_DESCRIPTION***' INTO s1 s2 s3.
        o_dtbgroup_description = s2.
        o_dtbgroup_code        = i_dtbgroup_code.


    CATCH cx_web_message_error INTO DATA(lx_web_message_error).
      " Handle Exception
*      RAISE SHORTDUMP lx_web_message_error.

    CATCH cx_abap_context_info_error INTO DATA(lx_abap_context_info_error).
      " Handle Exception
*      RAISE SHORTDUMP lx_abap_context_info_error.

    CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
      " Handle remote Exception
*      RAISE SHORTDUMP lx_remote.

    CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
      " Handle Exception
*      RAISE SHORTDUMP lx_gateway.

    CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
      " Handle Exception
*      RAISE SHORTDUMP lx_web_http_client_error.

    CATCH cx_http_dest_provider_error INTO DATA(lx_http_dest_provider_error).
        "handle exception
*      RAISE SHORTDUMP lx_http_dest_provider_error.

    ENDTRY.

  ENDMETHOD. " get_custom_fields_internal

  METHOD get_custom_fields_opt_internal. " Get Description for Article, Color, Pricat, Series, DTB Group (via Custom Business Object ODATA API) - optimized

*   potrebujem v PRICAT aby ste:
*   vymazali API call na custom fields cize desciption netreba
*   vymazali fieldy vsetky desciption aby neboli na screene
*   vymazali ten field na splittovanie "Row limit"
*   v Release XML aby ste pridali SERIES_CODE a vsetky desciption vymazali
    o_article_description   = ''.
    o_article_code          = i_article_code.
    o_color_description     = ''.
    o_color_code            = i_color_code.
    o_pricat_description    = ''.
    o_pricat_code           = i_pricat_code.
    o_series_description    = ''.
    o_series_code           = i_series_code.
    o_dtbgroup_description  = ''.
    o_dtbgroup_code         = i_dtbgroup_code.
    RETURN.

*   DATA it_cash_create TYPE TABLE FOR CREATE zi_cash_006. " Cash

    IF ( i_article_code IS NOT INITIAL ).
        READ TABLE it_article INTO DATA(article) WITH KEY code = i_article_code BINARY SEARCH.
*        SELECT SINGLE * FROM zi_cash_006 WHERE ( type = 'A' ) AND ( code = @i_article_code ) INTO @DATA(article).
        IF ( sy-subrc = 0 ).
            o_article_description   = article-description.
            o_article_code          = article-code.
        ENDIF.
    ENDIF.
    IF ( i_color_code IS NOT INITIAL ).
        READ TABLE it_color INTO DATA(color) WITH KEY code = i_color_code BINARY SEARCH.
*        SELECT SINGLE * FROM zi_cash_006 WHERE ( type = 'C' ) AND ( code = @i_color_code ) INTO @DATA(color).
        IF ( sy-subrc = 0 ).
            o_color_description   = color-description.
            o_color_code          = color-code.
        ENDIF.
    ENDIF.
    IF ( i_pricat_code IS NOT INITIAL ).
        READ TABLE it_pricat INTO DATA(pricat) WITH KEY code = i_pricat_code BINARY SEARCH.
*        SELECT SINGLE * FROM zi_cash_006 WHERE ( type = 'P' ) AND ( code = @i_pricat_code ) INTO @DATA(pricat).
        IF ( sy-subrc = 0 ).
            o_pricat_description   = pricat-description.
            o_pricat_code          = pricat-code.
        ENDIF.
    ENDIF.
    IF ( i_series_code IS NOT INITIAL ).
        READ TABLE it_series  INTO DATA(series) WITH KEY code = i_series_code BINARY SEARCH.
*        SELECT SINGLE * FROM zi_cash_006 WHERE ( type = 'S' ) AND ( code = @i_series_code ) INTO @DATA(series).
        IF ( sy-subrc = 0 ).
            o_series_description   = series-description.
            o_series_code          = series-code.
        ENDIF.
    ENDIF.
    IF ( i_dtbgroup_code IS NOT INITIAL ).
        READ TABLE it_dtbgroup  INTO DATA(dtbgroup) WITH KEY code = i_dtbgroup_code BINARY SEARCH.
        IF ( sy-subrc = 0 ).
            o_dtbgroup_description  = dtbgroup-description.
            o_dtbgroup_code         = dtbgroup-code.
        ENDIF.
    ENDIF.

*   If did not find descriptions in cash
    DATA(article_found) = abap_true.
    IF ( ( i_article_code IS NOT INITIAL ) AND ( o_article_code IS INITIAL ) ).
        article_found = abap_false.
    ENDIF.
    DATA(color_found) = abap_true.
    IF ( ( i_color_code IS NOT INITIAL ) AND ( o_color_code IS INITIAL ) ).
        color_found = abap_false.
    ENDIF.
    DATA(pricat_found) = abap_true.
    IF ( ( i_pricat_code IS NOT INITIAL ) AND ( o_pricat_code IS INITIAL ) ).
        pricat_found = abap_false.
    ENDIF.
    DATA(series_found) = abap_true.
    IF ( ( i_series_code IS NOT INITIAL ) AND ( o_series_code IS INITIAL ) ).
        series_found = abap_false.
    ENDIF.
    DATA(dtbgroup_found) = abap_true.
    IF ( ( i_dtbgroup_code IS NOT INITIAL ) AND ( o_dtbgroup_code IS INITIAL ) ).
        dtbgroup_found = abap_false.
    ENDIF.
*   If did not find descriptions in cash - call API
    IF ( ( article_found = abap_false ) OR ( color_found = abap_false ) OR ( pricat_found = abap_false ) OR ( series_found = abap_false ) OR ( dtbgroup_found = abap_false ) ).

        zbp_i_pricat_006=>get_custom_fields_internal(
          EXPORTING
             i_article_code         = i_article_code    " '123'
             i_color_code           = i_color_code      " '030'
             i_pricat_code          = i_pricat_code     " '21'
             i_series_code          = i_series_code     " '126'
             i_dtbgroup_code        = i_dtbgroup_code   " '114'
          IMPORTING
             o_article_description  = o_article_description
             o_color_description    = o_color_description
             o_pricat_description   = o_pricat_description
             o_series_description   = o_series_description
             o_dtbgroup_description = o_dtbgroup_description
             o_article_code         = o_article_code
             o_color_code           = o_color_code
             o_pricat_code          = o_pricat_code
             o_series_code          = o_series_code
             o_dtbgroup_code        = o_dtbgroup_code
        ).

    ENDIF.
    IF ( ( article_found = abap_false ) AND ( o_article_code IS NOT INITIAL ) ). " found via API
        APPEND VALUE descr_table( code = o_article_code description = o_article_description ) TO it_article.
        SORT it_article STABLE BY code.
    ENDIF.
    IF ( ( color_found = abap_false ) AND ( o_color_code IS NOT INITIAL ) ). " found via API
        APPEND VALUE descr_table( code = o_color_code description = o_color_description ) TO it_color.
        SORT it_color STABLE BY code.
    ENDIF.
    IF ( ( pricat_found = abap_false ) AND ( o_pricat_code IS NOT INITIAL ) ). " found via API
        APPEND VALUE descr_table( code = o_pricat_code description = o_pricat_description ) TO it_pricat.
        SORT it_pricat STABLE BY code.
    ENDIF.
    IF ( ( series_found = abap_false ) AND ( o_series_code IS NOT INITIAL ) ). " found via API
        APPEND VALUE descr_table( code = o_series_code description = o_series_description ) TO it_series.
        SORT it_series STABLE BY code.
    ENDIF.
    IF ( ( dtbgroup_found = abap_false ) AND ( o_dtbgroup_code IS NOT INITIAL ) ). " found via API
        APPEND VALUE descr_table( code = o_dtbgroup_code description = o_dtbgroup_description ) TO it_dtbgroup.
        SORT it_dtbgroup STABLE BY code.
    ENDIF.

  ENDMETHOD. " get_custom_fields_opt_internal

  METHOD enrich_product_row_internal. " Enrich Product Row With Product Data

        IF ( i_product-Product IS INITIAL ).
            RETURN.
        ENDIF.

*        Get Custom Fields (works in On Product Modify Event only)
*        DATA(i_article_code)    = CONV string( i_product-YY1_SeriesArticleGroup_PRD ).    " '123'
*        DATA(i_color_code)      = CONV string( i_product-YY1_Color_PRD ).                 " '030'
*        DATA(i_pricat_code)     = CONV string( i_product-YY1_PRICATGroup_PRD ).           " '21'
*        DATA(i_series_code)     = CONV string( i_product-YY1_SeriesName_PRD ).            " '126'
*        DATA(i_dtbgroup_code)   = CONV string( i_product-YY1_DTBGroup_PRD ).              " '114'
*        zbp_i_pricat_006=>get_custom_fields_opt_internal(
*          EXPORTING
*             i_article_code         = i_article_code
*             i_color_code           = i_color_code
*             i_pricat_code          = i_pricat_code
*             i_series_code          = i_series_code
*             i_dtbgroup_code        = i_dtbgroup_code
*          IMPORTING
*             o_article_description  = DATA(article_description)
*             o_color_description    = DATA(color_description)
*             o_pricat_description   = DATA(pricat_description)
*             o_series_description   = DATA(series_description)
*             o_dtbgroup_description = DATA(dtbgroup_description)
*             o_article_code         = DATA(article_code)
*             o_color_code           = DATA(color_code)
*             o_pricat_code          = DATA(pricat_code)
*             o_series_code          = DATA(series_code)
*             o_dtbgroup_code        = DATA(dtbgroup_code)
*        ).

*       Sales Price - AAA: Sales Price - ???
        DATA(salesPrice)    = 0.

*       Retail Price - AAE: Retail Price - ??? (maybe A_ProductValuation/to_ValuationCosting as StandardPrice) manage priceS - sales ZRRP + ZRR2 = 9 (1010)
        DATA(retailPrice)   = 0.

*       Product ID
        o_product-ProductID          = i_product-Product.

*       Article - YY1_SeriesArticleGroup_PRD
        o_product-Article            = i_product-YY1_SeriesArticleGroup_PRD.

*       Article Name - Article Name - YY1_SeriesArticleGroup_PRDT
*        o_product-ArticleName        = article_description.

*       Pricat Group Number - PRICAT Group Number - ??? YY1_PRICATGroupNo_PRD (fixed to YY1_PRICATGroup_PRD)
        o_product-PricatGroupNumber  = i_product-YY1_PRICATGroup_PRD.

*       Pricat Name - PRICAT Name - YY1_PRICATGroupNo_PRDT (fixed to YY1_PRICATGroup_PRDT)
*        o_product-PricatName         = pricat_description.

*       Series - YY1_SeriesName_PRD
        o_product-Series             = i_product-YY1_SeriesName_PRD.

*       Series Name - Series Name - YY1_SeriesName_PRDT
*        <entity>-SeriesName         = series_description.

        SPLIT i_product-Product AT '-' INTO DATA(s1) DATA(s2) DATA(s3) DATA(s4).

*       BackSize - Size - ??? in A_Product as YY1_SizeFR_PRD, YY1_SizeUS_PRD, YY1_SizeGB_PRD, Product (substring - 4th)
        o_product-BackSize           = s4.

*       CupSize - Cup - ??? in A_Product as Product (substring - 3rd)
        o_product-CupSize            = s3.

*       Color - YY1_Color_PRD
        o_product-Color              = i_product-YY1_Color_PRD.

*       ColorName - Color name - YY1_Color_PRDT
*        <entity>-ColorName          = color_description.

*       GTIN - GTIN - in A_Product as ProductStandardID
        o_product-GTIN               = i_product-ProductStandardID.

*       ProductGroup - MateialGroup (example: Z00001318)
        o_product-ProductGroup       = i_product-ProductGroup.

*       ProductName - Product Desciption ('EN')
        SELECT SINGLE * FROM I_ProductDescription WHERE ( Product = @i_product-Product ) AND ( Language = 'E' ) INTO @DATA(productDescription).
        IF ( sy-subrc = 0 ).
            o_product-ProductName = productDescription-ProductDescription.
        ELSE.
            o_product-ProductName = ''.
        ENDIF.

*       SalesStatus (Cross-Distribution Chain Product Status)
        o_product-SalesStatus    = i_product-SalesStatus.

*       ProductType
        o_product-ProductType    = i_product-ProductType.

*       ZCollection
        o_product-ZCollection    = i_product-YY1_Collection_PRD.

*       DTB Group
        o_product-DTBGroup       = i_product-YY1_DTBGroup_PRD.

*       DTB Group Name
*        o_product-DTBGroupName   = dtbgroup_description.

*       ProductURL (link to Product)
        o_product-ProductURL = '/ui#Material-manage&/C_Product(Product=''' && i_product-Product && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.

  ENDMETHOD. " enrich_product_row_internal

ENDCLASS. " zbp_i_pricat_006 IMPLEMENTATION
