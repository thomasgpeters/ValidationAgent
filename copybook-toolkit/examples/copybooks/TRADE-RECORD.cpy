       01  TRADE-RECORD.
           05  TRADE-ID                PIC X(16).
           05  TRADE-DATE              PIC X(10).
           05  TRADE-TIME              PIC X(8).
           05  TRADE-TYPE              PIC X(4).
           05  SYMBOL                  PIC X(10).
           05  QUANTITY                PIC 9(9).
           05  PRICE                   PIC 9(7)V9(4).
           05  TOTAL-AMOUNT            PIC S9(11)V99.
           05  COMMISSION              PIC 9(7)V99.
           05  BROKER-INFO.
               10  BROKER-ID           PIC X(8).
               10  BROKER-NAME         PIC X(30).
           05  CUSTOMER-INFO.
               10  CUSTOMER-ID         PIC X(12).
               10  CUSTOMER-NAME       PIC X(40).
           05  SETTLEMENT-DATE         PIC X(10).
           05  STATUS                  PIC X(2).
