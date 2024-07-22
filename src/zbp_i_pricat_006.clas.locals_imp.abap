CLASS lhc_product DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS on_product_modify FOR DETERMINE ON MODIFY IMPORTING keys FOR Product~on_product_modify.

******** Internal Methods *********

*   Get Description for Article, Color, Pricat, Series (via Custom Business Object ODATA API)
    METHODS get_custom_fields_internal
        IMPORTING VALUE(i_article_code)         TYPE string OPTIONAL
                  VALUE(i_color_code)           TYPE string OPTIONAL
                  VALUE(i_pricat_code)          TYPE string OPTIONAL
                  VALUE(i_series_code)          TYPE string OPTIONAL
        EXPORTING VALUE(o_article_description)  TYPE string
                  VALUE(o_color_description)    TYPE string
                  VALUE(o_pricat_description)   TYPE string
                  VALUE(o_series_description)   TYPE string
                  VALUE(o_article_code)         TYPE string
                  VALUE(o_color_code)           TYPE string
                  VALUE(o_pricat_code)          TYPE string
                  VALUE(o_series_code)          TYPE string.

ENDCLASS. " lhc_product DEFINITION

CLASS lhc_product IMPLEMENTATION.

  METHOD on_product_modify.

*    IF ( zbp_i_pricat_006=>on_product_modify_disable = abap_true ).
*        RETURN.
*    ENDIF.

    DATA it_product_update TYPE TABLE FOR UPDATE zi_pricat_006\\Product. " Product (item)

     " Read transfered instances
    READ ENTITIES OF zi_pricat_006 IN LOCAL MODE
        ENTITY Product
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        IF ( <entity>-%is_draft = '00' ). " Saved
        ENDIF.
        IF ( <entity>-%is_draft = '01' ). " Draft
        ENDIF.

*       Read pricat (root)
        READ ENTITIES OF zi_pricat_006 IN LOCAL MODE
            ENTITY Pricat
            ALL FIELDS WITH VALUE #( (
                %is_draft   = <entity>-%is_draft
                PricatUUID  = <entity>-PricatUUID
            ) )
            RESULT DATA(lt_pricat)
            FAILED DATA(failed1)
            REPORTED DATA(reported1).
        READ TABLE lt_pricat INTO DATA(pricat) INDEX 1.

        DATA(productID) = |{ <entity>-ProductID ALPHA = IN }|.

*       Product

*       Product(s):
        DATA product TYPE I_Product.
        CLEAR product.
        IF ( <entity>-ProductID IS NOT INITIAL ).
            SELECT SINGLE * FROM I_Product WHERE ( Product = @<entity>-ProductID ) INTO @product. " '0000301-030-C-080'
        ENDIF.

        IF ( product-Product IS NOT INITIAL ).

            DATA(i_article_code)    = CONV string( product-YY1_SeriesArticleGroup_PRD ).    " '123'
            DATA(i_color_code)      = CONV string( product-YY1_Color_PRD ).                 " '030'
            DATA(i_pricat_code)     = CONV string( product-YY1_PRICATGroup_PRD ).           " '21'
            DATA(i_series_code)     = CONV string( product-YY1_SeriesName_PRD ).            " '126'
            DATA(i_dtbgroup_code)   = CONV string( product-YY1_DTBGroup_PRD ).              " '114'

*            get_custom_fields_internal(
            zbp_i_pricat_006=>get_custom_fields_opt_internal(
              EXPORTING
                 i_article_code         = i_article_code
                 i_color_code           = i_color_code
                 i_pricat_code          = i_pricat_code
                 i_series_code          = i_series_code
                 i_dtbgroup_code        = i_dtbgroup_code
              IMPORTING
                 o_article_description  = DATA(article_description)
                 o_color_description    = DATA(color_description)
                 o_pricat_description   = DATA(pricat_description)
                 o_series_description   = DATA(series_description)
                 o_dtbgroup_description = DATA(dtbgroup_description)
                 o_article_code         = DATA(article_code)
                 o_color_code           = DATA(color_code)
                 o_pricat_code          = DATA(pricat_code)
                 o_series_code          = DATA(series_code)
                 o_dtbgroup_code        = DATA(dtbgroup_code)
            ).

        ENDIF.

*       Sales Price - AAA: Sales Price - ???
        DATA(salesPrice)    = 0.

*       Retail Price - AAE: Retail Price - ??? (maybe A_ProductValuation/to_ValuationCosting as StandardPrice) manage priceS - sales ZRRP + ZRR2 = 9 (1010)
        DATA(retailPrice)   = 0.

*       Article Name - Article Name - YY1_SeriesArticleGroup_PRDT
        <entity>-Article            = i_article_code.

*       Article Name - Article Name - YY1_SeriesArticleGroup_PRDT
        <entity>-ArticleName        = article_description.

*       Pricat Group Number - PRICAT Group Number - ??? YY1_PRICATGroupNo_PRD (fixed to YY1_PRICATGroup_PRD)
        <entity>-PricatGroupNumber  = product-YY1_PRICATGroup_PRD.

*       Pricat Name - PRICAT Name - YY1_PRICATGroupNo_PRDT (fixed to YY1_PRICATGroup_PRDT)
        <entity>-PricatName         = pricat_description.

*       Series - Series - YY1_SeriesName_PRDT
        <entity>-Series             = i_series_code.

*       Series Name - Series Name - YY1_SeriesName_PRDT
        <entity>-SeriesName         = series_description.

        SPLIT product-Product AT '-' INTO DATA(s1) DATA(s2) DATA(s3) DATA(s4).

*       BackSize - Size - ??? in A_Product as YY1_SizeFR_PRD, YY1_SizeUS_PRD, YY1_SizeGB_PRD, Product (substring - 4th)
        <entity>-BackSize           = s4.

*       CupSize - Cup - ??? in A_Product as Product (substring - 3rd)
        <entity>-CupSize            = s3.

*       Color - Color - YY1_Color_PRD
        <entity>-Color              = product-YY1_Color_PRD.

*       ColorName - Color name - YY1_Color_PRDT
        <entity>-ColorName          = color_description.

*       GTIN - GTIN - in A_Product as ProductStandardID
        <entity>-GTIN               = product-ProductStandardID.

*       ProductGroup - MateialGroup (example: Z00001318)
        <entity>-ProductGroup       = product-ProductGroup.

*       ProductName - Product Desciption ('EN')
        SELECT SINGLE * FROM I_ProductDescription WHERE ( Product = @<entity>-ProductID ) AND ( Language = 'E' ) INTO @DATA(productDescription).
        IF ( sy-subrc = 0 ).
            <entity>-ProductName = productDescription-ProductDescription.
        ELSE.
            <entity>-ProductName = ''.
        ENDIF.

*       SalesStatus (Cross-Distribution Chain Product Status)
        <entity>-SalesStatus    = product-SalesStatus.

*       ProductType
        <entity>-ProductType    = product-ProductType.

*       ZCollection
        <entity>-ZCollection    = product-YY1_Collection_PRD.

*       DTB Group
        <entity>-DTBGroup       = product-YY1_DTBGroup_PRD.

*       DTB Group Name
        <entity>-DTBGroupName   = dtbgroup_description.

*       ProductURL (link to Product)
        IF ( <entity>-ProductID IS NOT INITIAL ).
            <entity>-ProductURL = '/ui#Material-manage&/C_Product(Product=''' && <entity>-ProductID && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.
        ELSE.
            <entity>-ProductURL = ''.
        ENDIF.

        APPEND VALUE #(
            %tky        = <entity>-%tky
            PricatGroupNumber   = <entity>-PricatGroupNumber
            PricatName          = <entity>-PricatName
            Series              = <entity>-Series
            SeriesName          = <entity>-SeriesName
            Article             = <entity>-Article
            ArticleName         = <entity>-ArticleName
            Color               = <entity>-Color
            ColorName           = <entity>-ColorName
            BackSize            = <entity>-BackSize
            CupSize             = <entity>-CupSize
            GTIN                = <entity>-GTIN
            ProductGroup        = <entity>-ProductGroup
            ProductName         = <entity>-ProductName
            SalesStatus         = <entity>-SalesStatus
            ProductType         = <entity>-ProductType
            ZCollection         = <entity>-ZCollection
            DTBGroup            = <entity>-DTBGroup
            DTBGroupName        = <entity>-DTBGroupName
            ProductURL          = <entity>-ProductURL
         )
         TO it_product_update.

        MODIFY ENTITIES OF zi_pricat_006 IN LOCAL MODE
            ENTITY Product
            UPDATE FIELDS (
                PricatGroupNumber PricatName Series SeriesName Article ArticleName Color ColorName BackSize CupSize GTIN ProductGroup ProductName SalesStatus ProductType ZCollection DTBGroup DTBGroupName ProductURL
            )
            WITH it_product_update
            FAILED DATA(failed2)
            MAPPED DATA(mapped2)
            REPORTED DATA(reported2).

    ENDLOOP.

  ENDMETHOD. " on_product_modify

*   Get Description for Article, Color, Pricat, Series (via Custom Business Object ODATA API)
  METHOD get_custom_fields_internal.

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
                '"SERIES_CODE":"' i_series_code '"'
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

ENDCLASS. " lhc_product IMPLEMENTATION

CLASS lhc_customer DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS on_customer_modify FOR DETERMINE ON MODIFY IMPORTING keys FOR Customer~on_customer_modify.

ENDCLASS. " lhc_customer DEFINITION

CLASS lhc_customer IMPLEMENTATION.

  METHOD on_customer_modify.

    DATA it_customer_update TYPE TABLE FOR UPDATE zi_pricat_006\\Customer. " Customer (item)

     " Read transfered instances
    READ ENTITIES OF zi_pricat_006 IN LOCAL MODE
        ENTITY Customer
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        IF ( <entity>-%is_draft = '00' ). " Saved
        ENDIF.
        IF ( <entity>-%is_draft = '01' ). " Draft
        ENDIF.

*       Read pricat (root)
        READ ENTITIES OF zi_pricat_006 IN LOCAL MODE
            ENTITY Pricat
            ALL FIELDS WITH VALUE #( (
                %is_draft   = <entity>-%is_draft
                PricatUUID  = <entity>-PricatUUID
            ) )
            RESULT DATA(lt_pricat)
            FAILED DATA(failed1)
            REPORTED DATA(reported1).
        READ TABLE lt_pricat INTO DATA(pricat) INDEX 1.

*Customer(s):

        DATA(customerID) = |{ <entity>-CustomerID ALPHA = IN }|.

*       Customer To Business Partner
        SELECT SINGLE * FROM I_CustomerToBusinessPartner WHERE ( Customer = @customerID ) INTO @DATA(customerToBusinessPartner).

*       Business Partner
        SELECT SINGLE * FROM I_BusinessPartner WHERE ( BusinessPartnerUUID = @customerToBusinessPartner-BusinessPartnerUUID ) INTO @DATA(businessPartner).

*       Currency - in A_Customer/to_CustomerSalesArea
        SELECT SINGLE
                *
            FROM
                I_CustomerSalesArea
            WHERE
                ( Customer              = @customerID                   ) AND
                ( SalesOrganization     = @pricat-SalesOrganization     ) AND " '1000'
                ( DistributionChannel   = @pricat-DistributionChannel   ) AND " '10'
                ( Division              = @pricat-Division              )     " '00'
            INTO
                @DATA(customerSalesArea).
        IF ( sy-subrc = 0 ).
            <entity>-Currency = customerSalesArea-Currency.
        ELSE.
            <entity>-Currency = ''.
        ENDIF.

*       CountryCode - in A_BusinessPartnerAddress as Country
        SELECT SINGLE
                *
            FROM
                I_BusinessPartnerAddressTP_3
            WHERE
                ( BusinessPartner = @businessPartner-BusinessPartner )
            INTO
                @DATA(businessPartnerAddress).
        IF ( sy-subrc = 0 ).
            <entity>-Country = businessPartnerAddress-Country.
        ELSE.
            <entity>-Country = ''.
        ENDIF.

*       GLN - in A_BusinessPartnerIdentification "BUP005"
        SELECT SINGLE
                *
            FROM
                I_BuPaIdentification
            WHERE
                ( BusinessPartner       = @businessPartner-BusinessPartner  ) AND " '0010000664'
                ( BPIdentificationType  = 'BUP005'                          )
            INTO
                @DATA(buPaIdentification).
        IF ( sy-subrc = 0 ).
            <entity>-GLN = buPaIdentification-BPIdentificationNumber.
        ELSE.
            <entity>-GLN = ''.
        ENDIF.

*       CustomerURL (link to Customer)
        <entity>-CustomerURL = '/ui#Customer-manage&/C_BusinessPartnerCustomer(BusinessPartner=''' && <entity>-CustomerID && ''',DraftUUID=guid''00000000-0000-0000-0000-000000000000'',IsActiveEntity=true)'.

        APPEND VALUE #(
            %tky        = <entity>-%tky
            Currency    = <entity>-Currency
            Country     = <entity>-Country
            GLN         = <entity>-GLN
            CustomerURL = <entity>-CustomerURL
         )
         TO it_customer_update.

        MODIFY ENTITIES OF zi_pricat_006 IN LOCAL MODE
            ENTITY Customer
            UPDATE FIELDS ( Currency Country GLN CustomerURL )
            WITH it_customer_update
            FAILED DATA(failed2)
            MAPPED DATA(mapped2)
            REPORTED DATA(reported2).

    ENDLOOP.

  ENDMETHOD. " on_customer_modify

ENDCLASS. " lhc_customer IMPLEMENTATION

CLASS lhc_pricat DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR pricat RESULT result.

    METHODS activate FOR MODIFY
      IMPORTING keys FOR ACTION pricat~activate.

    METHODS edit FOR MODIFY
      IMPORTING keys FOR ACTION pricat~edit.

    METHODS resume FOR MODIFY
      IMPORTING keys FOR ACTION pricat~resume.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR pricat RESULT result.

    METHODS release FOR MODIFY IMPORTING keys FOR ACTION Pricat~release.

    METHODS add_pricat_customers FOR MODIFY IMPORTING keys FOR ACTION Pricat~add_pricat_customers.

    METHODS add_promotion_customers FOR MODIFY IMPORTING keys FOR ACTION Pricat~add_promotion_customers.

    METHODS add_all_series FOR MODIFY IMPORTING keys FOR ACTION Pricat~add_all_series.

    METHODS add_products_based_on_filters FOR MODIFY IMPORTING keys FOR ACTION Pricat~add_products_based_on_filters.

******** Internal Methods *********

*   Get Description for Article, Color, Pricat, Series (via Custom Business Object ODATA API)
    METHODS get_custom_fields_internal
        IMPORTING VALUE(i_article_code)         TYPE string OPTIONAL
                  VALUE(i_color_code)           TYPE string OPTIONAL
                  VALUE(i_pricat_code)          TYPE string OPTIONAL
                  VALUE(i_series_code)          TYPE string OPTIONAL
        EXPORTING VALUE(o_article_description)  TYPE string
                  VALUE(o_color_description)    TYPE string
                  VALUE(o_pricat_description)   TYPE string
                  VALUE(o_series_description)   TYPE string
                  VALUE(o_article_code)         TYPE string
                  VALUE(o_color_code)           TYPE string
                  VALUE(o_pricat_code)          TYPE string
                  VALUE(o_series_code)          TYPE string.

ENDCLASS. " lhc_pricat DEFINITION

CLASS lhc_pricat IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD activate.

     " Read transfered instances
    READ ENTITIES OF zi_pricat_006 IN LOCAL MODE
        ENTITY Pricat
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        IF ( <entity>-%is_draft = '00' ). " Saved
        ENDIF.
        IF ( <entity>-%is_draft = '01' ). " Draft
        ENDIF.

*       Generate and Set New Pricat ID
        IF ( <entity>-PricatID IS INITIAL ).

            DATA pricatid TYPE zi_pricat_006-PricatID VALUE '0000000000'.
            SELECT MAX( pricatid ) FROM zi_pricat_006 INTO (@pricatid).
            pricatid  = ( pricatid + 1 ).

            MODIFY ENTITIES OF zi_pricat_006 IN LOCAL MODE
                ENTITY Pricat
                UPDATE FIELDS ( PricatID )
                WITH VALUE #( (
                    %tky        = <entity>-%tky
                    PricatID    = pricatid
                ) )
                FAILED DATA(ls_failed1)
                MAPPED DATA(ls_mapped1)
                REPORTED DATA(ls_reported1).

         ENDIF.

    ENDLOOP.

  ENDMETHOD. " activate

  METHOD edit.
  ENDMETHOD.

  METHOD resume.
  ENDMETHOD.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD release. "  " on Pressing Release button

    " Read transfered instances
    READ ENTITIES OF zi_pricat_006 IN LOCAL MODE
        ENTITY Pricat
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        IF ( <entity>-%is_draft = '00' ). " Saved

            IF ( <entity>-Released = abap_true ).
*               Short format message
                APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'The Price Catalog is already released.' ) ) TO reported-pricat.
                RETURN.
            ENDIF.

*           Customers
            READ ENTITIES OF zi_pricat_006  IN LOCAL MODE
                ENTITY Pricat BY \_Customer
                ALL FIELDS WITH VALUE #( (
                    %tky = <entity>-%tky
                ) )
                RESULT DATA(lt_customer)
                FAILED DATA(failed1)
                REPORTED DATA(reported1).

            IF ( lt_customer[] IS INITIAL ).
*               Short format message
*                APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'No Customer.' ) ) TO reported-pricat.
*                RETURN.
            ENDIF.

            SORT lt_customer STABLE BY CustomerID.

*           Products
            READ ENTITIES OF zi_pricat_006 IN LOCAL MODE
                ENTITY Pricat BY \_Product
                ALL FIELDS WITH VALUE #( (
                    %tky = <entity>-%tky
                ) )
                RESULT DATA(lt_product)
                FAILED DATA(failed2)
                REPORTED DATA(reported2).

            IF ( lt_product[] IS INITIAL ).
*               Short format message
*                APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'No Attachment.' ) ) TO reported-shipment.
*                RETURN.
            ENDIF.

            SORT lt_product STABLE BY ProductID.

            DATA request_body TYPE string VALUE ''.

*           Make body as an XML
            request_body = request_body && '<Pricat>' && cl_abap_char_utilities=>cr_lf.

            request_body = request_body && '<ID>' && <entity>-PricatID && '</ID>' && cl_abap_char_utilities=>cr_lf.
            request_body = request_body && '<SalesOrganization>' && <entity>-SalesOrganization && '</SalesOrganization>' && cl_abap_char_utilities=>cr_lf.
            request_body = request_body && '<DistributionChannel>' && <entity>-DistributionChannel && '</DistributionChannel>' && cl_abap_char_utilities=>cr_lf.
            request_body = request_body && '<Division>' && <entity>-Division && '</Division>' && cl_abap_char_utilities=>cr_lf.
            request_body = request_body && '<PricingDate>' && <entity>-PricingDate && '</PricingDate>' && cl_abap_char_utilities=>cr_lf.

            DATA customerID        TYPE string.
            LOOP AT lt_customer INTO DATA(customer).
                customerID         = |{ customer-CustomerID ALPHA = OUT }|.
                CONDENSE customerID NO-GAPS.
                request_body = request_body && '<Customer>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<CustomerID>' && customerID && '</CustomerID>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<Currency>' && customer-Currency && '</Currency>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<CountryCode>' && customer-Country && '</CountryCode>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<GLN>' && customer-GLN && '</GLN>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '</Customer>' && cl_abap_char_utilities=>cr_lf.
            ENDLOOP.

            DATA productID TYPE string.
            LOOP AT lt_product INTO DATA(product).
                productID      = |{ product-ProductID ALPHA = OUT }|.
                CONDENSE productID NO-GAPS.
                request_body = request_body && '<Product>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<ProductID>' && productID && '</ProductID>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<PricatGroupNumber>' && product-PricatGroupNumber && '</PricatGroupNumber>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<PricatName>' && product-PricatName && '</PricatName>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<SeriesName>' && product-SeriesName && '</SeriesName>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<ArticleName>' && product-ArticleName && '</ArticleName>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<Color>' && product-Color && '</Color>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<ColorName>' && product-ColorName && '</ColorName>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<GTIN>' && product-GTIN && '</GTIN>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<MaterialGroup>' && product-ProductGroup && '</MaterialGroup>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '<ProductDescription>' && product-ProductName && '</ProductDescription>' && cl_abap_char_utilities=>cr_lf.
                request_body = request_body && '</Product>' && cl_abap_char_utilities=>cr_lf.
            ENDLOOP.

            request_body = request_body && '</Pricat>' && cl_abap_char_utilities=>cr_lf.

*           Do Free Style HTTP Request
            TRY.

                DATA i_url         TYPE string VALUE 'https://felina-hu-scpi-test-eyjk96r2.it-cpi018-rt.cfapps.eu10-003.hana.ondemand.com/http/FiegeOutboundDevCust'.
                DATA i_username    TYPE string VALUE 'sb-1e950f89-c676-4acd-b0dc-24e58f8aab45!b143168|it-rt-felina-hu-scpi-test-eyjk96r2!b117912'.
                DATA i_password    TYPE string VALUE 'cc744b1f-5237-4a7e-ab44-858fdd00fb73$3wcTQpYfe1kbmjltnA8zSDb5ogj0TpaYon4WHM-TwfE='.

                DATA(system_url) = cl_abap_context_info=>get_system_url( ).
                IF ( system_url(8) = 'my404898' ). " dev-cust
                    i_url = 'https://felina-hu-scpi-test-eyjk96r2.it-cpi018-rt.cfapps.eu10-003.hana.ondemand.com/http/FiegeOutboundDevCust'.
                ENDIF.
                IF ( system_url(8) = 'my404907' ). " test
                    i_url = 'https://felina-hu-scpi-test-eyjk96r2.it-cpi018-rt.cfapps.eu10-003.hana.ondemand.com/http/Pricat'.
                ENDIF.

*PRICAT na realease:
*URL: https://felinahuscpi.it-cpi001-rt.cfapps.eu10.hana.ondemand.com/http/Pricat
*username: sb-d6f68eaf-c42b-4ab0-931d-60753301674e!b102052|it-rt-felinahuscpi!b16077
*passwrod: 462ff083-e299-4fb6-a44f-3b627fd8b406$XlkjK6-n64zyzYZkjk45eANimRNA-nMD8Pe3TKppq9w=
                IF ( system_url(8) = 'my410080' ). " prod
                    i_url       = 'https://felinahuscpi.it-cpi001-rt.cfapps.eu10.hana.ondemand.com/http/Pricat'.
                    i_username  = 'sb-d6f68eaf-c42b-4ab0-931d-60753301674e!b102052|it-rt-felinahuscpi!b16077'.
                    i_password  = '462ff083-e299-4fb6-a44f-3b627fd8b406$XlkjK6-n64zyzYZkjk45eANimRNA-nMD8Pe3TKppq9w='.
                ENDIF.

                DATA(http_destination) = cl_http_destination_provider=>create_by_url(
                    i_url = i_url
                ).

                DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination(
                    i_destination = http_destination
                ).

                lo_http_client->get_http_request( )->set_authorization_basic(
                    i_username = i_username
                    i_password = i_password
                ).

                lo_http_client->get_http_request( )->set_text(
                    i_text = request_body " 'Hello, CPI!'
                ).

                DATA(lo_http_response) = lo_http_client->execute(
                    i_method   = if_web_http_client=>get
                    i_timeout  = 1
                ).

*                DATA(response_body) = lo_http_response->get_text( ).
*                DATA(status)        = lo_http_response->get_status( ).
*                DATA(header_fields) = lo_http_response->get_header_fields( ).
*                DATA(header_status) = lo_http_response->get_header_field( '~status_code' ).

*                out->write( cl_abap_char_utilities=>cr_lf && status-code && cl_abap_char_utilities=>cr_lf ).
*                out->write( cl_abap_char_utilities=>cr_lf && response_body && cl_abap_char_utilities=>cr_lf ).

*                IF ( status-code = 200 ).
                    APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success text = 'Successfully Sent.'  ) ) TO reported-pricat.
*                ELSE.
*                    DATA(code) = CONV string( status-code ).
*                    CONCATENATE 'Error Status Code =' code '.' INTO DATA(text) SEPARATED BY space.
*                    APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = text  ) ) TO reported-pricat.
*                    RETURN.
*                ENDIF.

            CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
              " Handle remote Exception
*              RAISE SHORTDUMP lx_remote.
                APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Remote Error.' ) ) TO reported-pricat.
                RETURN.

            CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
              " Handle Exception
*              RAISE SHORTDUMP lx_gateway.
                APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Gateway Error.' ) ) TO reported-pricat.
                RETURN.

            CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
              " Handle Exception
*              RAISE SHORTDUMP lx_web_http_client_error.
*               APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Web HTTP Client Error.' ) ) TO reported-pricat.
*               RETURN.
                APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success text = 'Process in CPI started!.'  ) ) TO reported-pricat.

            CATCH cx_http_dest_provider_error INTO DATA(lx_http_dest_provider_error).
                "handle exception
*              RAISE SHORTDUMP lx_http_dest_provider_error.
                APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'HTTP Dest Provider Error.' ) ) TO reported-pricat.
                RETURN.

            CATCH cx_abap_context_info_error INTO DATA(lx_abap_context_info_error).
                "handle exception
*              RAISE SHORTDUMP lx_abap_context_info_error.
                APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'ABAP Context Info Error.' ) ) TO reported-pricat.
                RETURN.

            ENDTRY.

            MODIFY ENTITIES OF zi_pricat_006 IN LOCAL MODE
                ENTITY Pricat
                UPDATE FIELDS ( Released )
                WITH VALUE #( (
                    %tky        = <entity>-%tky
                    Released    = abap_true
                ) )
                FAILED DATA(failed3)
                MAPPED DATA(mapped3)
                REPORTED DATA(reported3).

        ENDIF.

        IF ( <entity>-%is_draft = '01' ). " Draft

*           Short format message
            APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Data not saved.' ) ) TO reported-pricat.
            RETURN.

        ENDIF.

    ENDLOOP.

  ENDMETHOD. " release

  METHOD add_pricat_customers.

    DATA it_customer_create TYPE TABLE FOR CREATE zi_pricat_006\_Customer. " Customer (item)

    " Read transfered instances
    READ ENTITIES OF zi_pricat_006 IN LOCAL MODE
        ENTITY Pricat
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        IF ( <entity>-%is_draft = '00' ). " Saved
        ENDIF.
        IF ( <entity>-%is_draft = '01' ). " Draft
        ENDIF.

*        SELECT * FROM I_BusinessPartner WHERE ( YY1_PRICAT_bus = 'X' ) ORDER BY BusinessPartner INTO TABLE @DATA(it_businesspartner).
        SELECT * FROM I_CustomerToBusinessPartner WHERE ( \_BusinessPartner-YY1_PRICAT_bus = 'X' ) ORDER BY Customer INTO TABLE @DATA(it_customer).

        IF ( sy-subrc <> 0 ).
            APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'No Customer Found.' ) ) TO reported-pricat.
            RETURN.
        ENDIF.

        LOOP AT it_customer INTO DATA(customer).
            DATA(cid) = sy-tabix.
            DATA(customerID) = |{ customer-Customer ALPHA = OUT }|.
            APPEND VALUE #(
                %tky  = <entity>-%tky
                %target = VALUE #( (
                    %is_draft       = <entity>-%is_draft
                    %cid            = cid
                    CustomerID      = customerID
                ) )
            ) TO it_customer_create.
        ENDLOOP.

*       Create Customer (item)
        MODIFY ENTITIES OF zi_pricat_006 IN LOCAL MODE
            ENTITY Pricat
            CREATE BY \_Customer
            FIELDS ( CustomerID )
            WITH it_customer_create
            MAPPED DATA(mapped1)
            FAILED DATA(failed1)
            REPORTED DATA(reported1).

    ENDLOOP.

  ENDMETHOD. " add_pricat_customers

  METHOD add_promotion_customers.

    DATA it_customer_create TYPE TABLE FOR CREATE zi_pricat_006\_Customer. " Customer (item)

    " Read transfered instances
    READ ENTITIES OF zi_pricat_006 IN LOCAL MODE
        ENTITY Pricat
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        IF ( <entity>-%is_draft = '00' ). " Saved
        ENDIF.
        IF ( <entity>-%is_draft = '01' ). " Draft
        ENDIF.

*        SELECT * FROM I_Customer INTO TABLE @DATA(it_customer).
*        SELECT * FROM I_BusinessPartner WHERE ( YY1_PRICAT_bus = 'X' ) ORDER BY BusinessPartner INTO TABLE @DATA(it_businesspartner).
*        SELECT * FROM I_CustomerToBusinessPartner WHERE ( \_BusinessPartner-YY1_PRICAT_bus = 'X' ) ORDER BY Customer INTO TABLE @DATA(it_customer).

        ##ASSOC_TO_N_OK[_CUSTOMERSALESAREA]
        SELECT
                \_CustomerSalesArea-Customer AS Customer,
                \_CustomerSalesArea-SalesOrganization AS SalesOrganization,
                \_CustomerSalesArea-DistributionChannel AS DistributionChannel,
                \_CustomerSalesArea-Division AS Division
            FROM
                I_Customer
            WHERE
                ( \_CustomerSalesArea-SalesOrganization         = @<entity>-SalesOrganization   ) AND " '1000'
                ( \_CustomerSalesArea-DistributionChannel       = @<entity>-DistributionChannel ) AND " '10'
                ( \_CustomerSalesArea-Division                  = @<entity>-Division            ) AND " '00'
                ( \_CustomerSalesArea-AdditionalCustomerGroup2  = '1' )                               " '1' - Promotion Customer
            ORDER BY
                Customer
            INTO TABLE
                @DATA(it_customer).

*       Additional filtering by Pricat Customer
        SELECT * FROM I_CustomerToBusinessPartner WHERE ( \_BusinessPartner-YY1_PRICAT_bus = 'X' ) ORDER BY Customer INTO TABLE @DATA(it_customertobusinesspartner).
        SORT it_customertobusinesspartner BY Customer.
        LOOP AT it_customer ASSIGNING FIELD-SYMBOL(<fs_customer>).
            READ TABLE it_customertobusinesspartner WITH KEY Customer = <fs_customer>-Customer BINARY SEARCH TRANSPORTING NO FIELDS.
            IF ( sy-subrc <> 0 ).
                CLEAR <fs_customer>-Customer.
            ENDIF.
        ENDLOOP.
        DELETE it_customer WHERE ( Customer IS INITIAL ).

        IF ( it_customer[] IS INITIAL ).
            APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'No Customer Found.' ) ) TO reported-pricat.
            RETURN.
        ENDIF.

        LOOP AT it_customer INTO DATA(customer).
            DATA(cid) = sy-tabix.
            DATA(customerID) = |{ customer-Customer ALPHA = OUT }|.
            APPEND VALUE #(
                %tky  = <entity>-%tky
                %target = VALUE #( (
                    %is_draft       = <entity>-%is_draft
                    %cid            = cid
                    CustomerID      = customerID
                ) )
            ) TO it_customer_create.
        ENDLOOP.

*       Create Product (item)
        MODIFY ENTITIES OF zi_pricat_006 IN LOCAL MODE
            ENTITY Pricat
            CREATE BY \_Customer
            FIELDS ( CustomerID )
            WITH it_customer_create
            MAPPED DATA(mapped1)
            FAILED DATA(failed1)
            REPORTED DATA(reported1).

    ENDLOOP.


  ENDMETHOD. " add_promotion_customers

  METHOD add_all_series. " Add All Series

  ENDMETHOD. " add_all_series

  METHOD add_products_based_on_filters. " Add Product Based On Filters

    DATA it_product_create  TYPE TABLE FOR CREATE zi_pricat_006\_Product. " Product (item)
    DATA regexp             TYPE string.
    DATA r_producttype      TYPE RANGE OF I_Product-ProductType.
    DATA r_color            TYPE RANGE OF I_Product-YY1_Color_PRD.
    DATA r_collection       TYPE RANGE OF I_Product-YY1_Collection_PRD.
    DATA r_salesstatus      TYPE RANGE OF I_Product-SalesStatus.

    " Read transfered instances
    READ ENTITIES OF zi_pricat_006 IN LOCAL MODE
        ENTITY Pricat
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(entities).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

        IF ( <entity>-%is_draft = '00' ). " Saved
        ENDIF.
        IF ( <entity>-%is_draft = '01' ). " Draft
        ENDIF.

*        IF ( <entity>-ProductSeries IS INITIAL ).
*           APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'No Product Found.' ) ) TO reported-pricat.
*            RETURN.
*        ENDIF.

*       Filters:

*       Product Series
        CONDENSE <entity>-ProductSeries NO-GAPS.
        regexp = <entity>-ProductSeries && '%'.

*       Product Types
        CONDENSE <entity>-ProductTypes NO-GAPS.
        SPLIT <entity>-ProductTypes AT ',' INTO TABLE DATA(it_producttype).
        LOOP AT it_producttype INTO DATA(wa_producttype).
            APPEND VALUE #(
                sign    = 'I'
                option  = 'EQ'
                low     = wa_producttype
            )
            TO r_producttype.
        ENDLOOP.

*       Colors
        CONDENSE <entity>-Colors NO-GAPS.
        SPLIT <entity>-Colors AT ',' INTO TABLE DATA(it_color).
        LOOP AT it_color INTO DATA(wa_color).
            APPEND VALUE #(
                sign    = 'I'
                option  = 'EQ'
                low     = wa_color
            )
            TO r_color.
        ENDLOOP.

*       Collections
        CONDENSE <entity>-Collections NO-GAPS.
        SPLIT <entity>-Collections AT ',' INTO TABLE DATA(it_collection).
        LOOP AT it_collection INTO DATA(wa_collection).
            APPEND VALUE #(
                sign    = 'I'
                option  = 'EQ'
                low     = wa_collection
            )
            TO r_collection.
        ENDLOOP.

*       Sales Status
        CONDENSE <entity>-SalesStatus NO-GAPS.
        SPLIT <entity>-SalesStatus AT ',' INTO TABLE DATA(it_salesstatus).
        LOOP AT it_salesstatus INTO DATA(wa_salesstatus).
            APPEND VALUE #(
                sign    = 'I'
                option  = 'EQ'
                low     = wa_salesstatus
            )
            TO r_salesstatus.
        ENDLOOP.

*        SELECT * FROM I_ProductTP_2 WHERE ( Product LIKE @regexp ) INTO TABLE @DATA(it_product).
        SELECT
                *
            FROM
                I_Product
            WHERE
                ( Product               LIKE @regexp        ) AND
                ( ProductType           IN @r_producttype   ) AND
                ( YY1_Color_PRD         IN @r_color         ) AND
                ( YY1_Collection_PRD    IN @r_collection    ) AND
                ( SalesStatus           IN @r_salesstatus   )
            ORDER BY
                Product
            INTO TABLE
                @DATA(it_product).

        IF ( sy-subrc <> 0 ).
            APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'No Product Found.' ) ) TO reported-pricat.
            RETURN.
        ENDIF.

*       Read Product (items)
        READ ENTITIES OF zi_pricat_006 IN LOCAL MODE
            ENTITY Pricat BY \_Product
            ALL FIELDS WITH VALUE #( (
                %tky = <entity>-%tky
            ) )
            RESULT DATA(lt_product)
            FAILED DATA(failed1)
            REPORTED DATA(reported1).

*       Exclude Products presented in Items
        SORT lt_product STABLE BY ProductID.
        LOOP AT it_product ASSIGNING FIELD-SYMBOL(<product>).
            READ TABLE lt_product WITH KEY ProductID = <product>-Product BINARY SEARCH TRANSPORTING NO FIELDS.
            IF ( sy-subrc = 0 ).
                CLEAR <product>-Product.
            ENDIF.
        ENDLOOP.
        DELETE it_product WHERE ( Product iS INITIAL ).

*       Add rest Products into Items
        LOOP AT it_product INTO DATA(wa_product).
            DATA(tabix) = sy-tabix.
            DATA(cid)   = sy-tabix.
            APPEND VALUE #(
                %tky  = <entity>-%tky
                %target = VALUE #( (
                    %is_draft   = <entity>-%is_draft
                    %cid        = cid
                    ProductID   = wa_product-Product
                ) )
            ) TO it_product_create.

*           Row Limit
            IF ( <entity>-RowLimit IS NOT INITIAL ).
                IF ( tabix >= <entity>-RowLimit ).
                    DATA(added) = CONV string( tabix ).
                    DATA(outof) = CONV string( LINES( it_product ) ).
                    CONCATENATE 'Added' added 'Rows out of' outof INTO DATA(text) SEPARATED BY space.
                    APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-warning text = text ) ) TO reported-pricat.
                    EXIT.
                ENDIF.
            ENDIF.

        ENDLOOP.

*       Create Product (item)
        MODIFY ENTITIES OF zi_pricat_006 IN LOCAL MODE
            ENTITY Pricat
            CREATE BY \_Product
            FIELDS ( ProductID )
            WITH it_product_create
            MAPPED DATA(mapped2)
            FAILED DATA(failed2)
            REPORTED DATA(reported2).

    ENDLOOP.

  ENDMETHOD. " add_products_based_on_filters

* Get Description for Article, Color, Pricat, Series (via Custom Business Object ODATA API)
  METHOD get_custom_fields_internal.

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
                '"SERIES_CODE":"' i_series_code '"'
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

ENDCLASS. " lhc_pricat IMPLEMENTATION

CLASS lsc_zi_pricat_006 DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zi_pricat_006 IMPLEMENTATION.

  METHOD save_modified.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
