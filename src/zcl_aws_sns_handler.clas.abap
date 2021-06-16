CLASS zcl_aws_sns_handler DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .

    METHODS parse_notification
      IMPORTING
        !iv_post_body          TYPE string
      RETURNING
        VALUE(rv_notification) TYPE zaws_sns_notification .
    METHODS parse_sub_conf
      IMPORTING
        !iv_post_body      TYPE string
      RETURNING
        VALUE(rv_sub_conf) TYPE zaws_sns_sub_conf .
  PROTECTED SECTION.

    METHODS confirm_subscription
      IMPORTING
        !iv_subscribe_url TYPE string
        !iv_server        TYPE REF TO if_http_server .
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_aws_sns_handler IMPLEMENTATION.

  METHOD confirm_subscription.
    DATA: lv_message TYPE string.
    DATA: lo_http_client TYPE REF TO if_http_client.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = iv_subscribe_url
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO lv_message.
      iv_server->response->set_cdata( lv_message ).
      RETURN.
    ENDIF.

    lo_http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                  INTO lv_message.
      iv_server->response->set_cdata( lv_message ).
      RETURN.
    ENDIF.

    lo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                        INTO lv_message.
      iv_server->response->set_cdata( lv_message ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    DATA: lt_header_fields TYPE tihttpnvp.



    " When using SAP Cloud Platform Connectivity this header isn't passed to the Backend
    DATA(lv_aws_msg_type) = server->request->get_header_field( 'x-amz-sns-message-type' ).

    server->request->get_header_fields( CHANGING fields = lt_header_fields ).
    DATA(lv_post_body) = server->request->get_cdata( ).

    IF lv_aws_msg_type IS INITIAL.
      DATA(lv_sub_conf) = parse_sub_conf( lv_post_body ).
      lv_aws_msg_type = lv_sub_conf-type.
    ENDIF.

    LOG-POINT ID zaws_sns FIELDS lv_post_body lt_header_fields lv_aws_msg_type.

    IF lv_aws_msg_type = 'SubscriptionConfirmation'.

      IF lv_sub_conf IS INITIAL.
        lv_sub_conf = parse_sub_conf( lv_post_body ).
      ENDIF.

      LOG-POINT ID zaws_sns FIELDS lv_sub_conf.

      confirm_subscription(
          iv_subscribe_url = lv_sub_conf-subscribe_url
          iv_server        = server ).

    ELSEIF lv_aws_msg_type = 'Notification'.
      DATA(lv_notification) = parse_notification( lv_post_body ).
      LOG-POINT ID zaws_sns FIELDS lv_notification.
    ENDIF.

  ENDMETHOD.

  "! Parse Notification
  METHOD parse_notification.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json          = iv_post_body
        pretty_name   = abap_true
      CHANGING
        data          = rv_notification ).

  ENDMETHOD.


  METHOD parse_sub_conf.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json          = iv_post_body
        pretty_name   = abap_true
      CHANGING
        data          = rv_sub_conf ).

  ENDMETHOD.
ENDCLASS.