       01  ACCOUNT.
           05  COMPANY-NO              PIC 9(5).
           05  ACCOUNT-NO              PIC X(12).
           05  ACCOUNT-TYPE            PIC X(2).
           05  ACCOUNT-STATUS          PIC X(1).
           05  FILLER                  PIC X(5).
           05  BALANCE                 PIC S9(9)V99.
           05  CREDIT-LIMIT            PIC 9(9)V99.
           05  OPEN-DATE               PIC X(10).
           05  LAST-ACTIVITY-DATE      PIC X(10).
           05  CUSTOMER-NAME           PIC X(40).
           05  CUSTOMER-ADDRESS.
               10  ADDRESS-LINE-1      PIC X(30).
               10  ADDRESS-LINE-2      PIC X(30).
               10  CITY                PIC X(20).
               10  STATE               PIC X(2).
               10  ZIP-CODE            PIC X(10).
