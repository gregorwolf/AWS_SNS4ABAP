
CLASS zcl_aws_sns_handler_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>zcl_Aws_Sns_Handler_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_AWS_SNS_HANDLER
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_aws_sns_handler.  "class under test

    METHODS: parse_sub_conf FOR TESTING.
    METHODS: parse_notification FOR TESTING.
ENDCLASS.       "zcl_Aws_Sns_Handler_Test


CLASS zcl_aws_sns_handler_test IMPLEMENTATION.

  METHOD parse_sub_conf.

    DATA iv_post_body TYPE string.
    DATA rv_sub_conf TYPE zaws_sns_sub_conf.

    CREATE OBJECT f_cut.

    iv_post_body = '{ "Type" : "SubscriptionConfirmation",'.
    iv_post_body =  iv_post_body && '"MessageId" : "165545c9-2a5c-472c-8df2-7ff2be2b3b1b",'.
    iv_post_body =  iv_post_body && '"Token" : "2336412f37fb687f5d51e6e241d09c805a5a57b30d712f794cc5f6a988666d92768dd60a747ba6f3beb",'.
    iv_post_body =  iv_post_body && '"TopicArn" : "arn:aws:sns:us-west-2:123456789012:MyTopic",'.
    iv_post_body =  iv_post_body && '"Message" : "You have chosen to subscribe to the topic arn:aws:sns:us-west-2:123456789012:MyTopic.\nTo confirm the subscription, visit the SubscribeURL included in this message.",'.
    iv_post_body =  iv_post_body && '"SubscribeURL" : "https://sns.us-west-2.amazonaws.com/?Action=ConfirmSubscription&TopicArn=arn:aws:sns:us-w",'.
    iv_post_body =  iv_post_body && '"Timestamp" : "2012-04-26T20:45:04.751Z",'.
    iv_post_body =  iv_post_body && '"SignatureVersion" : "1",'.
    iv_post_body =  iv_post_body && '"Signature" : "EXAMPLEpH+DcEwjAPg8O9mY8dReBSwksfg2S7WKQcikcNKWLQjwu6A4VbeS0QHVCkhRS7fUQvi2egU3N858fiTDN6bkkOxYDVrY0Ad8L10Hs3zH81mtnPk5uvvolIC1CXGu43obcgFxeL3khZl8IKvO61GWB6jI9b5+gLPoBc1Q=",'.
    iv_post_body =  iv_post_body && '"SigningCertURL" : "https://sns.us-west-2.amazonaws.com/SimpleNotificationService-f3ecfb7224c7233fe7bb5f59f96de52f.pem"'.
    iv_post_body =  iv_post_body && '}'.

    rv_sub_conf = f_cut->parse_sub_conf( iv_post_body ).

    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act              = rv_sub_conf-token
    ).
    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act              = rv_sub_conf-subscribe_url
    ).

  ENDMETHOD.

  METHOD parse_notification.
    DATA iv_post_body TYPE string.
    DATA rv_notification TYPE zaws_sns_notification.

    CREATE OBJECT f_cut.

    iv_post_body = '{ "Type" : "Notification",'.
    iv_post_body =  iv_post_body && '"Message" : "{\"serialNumber\": \"G0355DSERDSFW\",\"batteryVoltage\": \"1340\",\"clickType\": \"SINGLE\"}"'.
    iv_post_body =  iv_post_body && '}'.

    rv_notification = f_cut->parse_notification( iv_post_body = iv_post_body ).

    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act              = rv_notification-message
    ).


  ENDMETHOD.

ENDCLASS.