
connect to cs157a@

drop procedure p2.cust_crt@
drop procedure p2.cust_login@
drop procedure p2.acct_opn@
drop procedure p2.acct_cls@
drop procedure p2.acct_dep@
drop procedure p2.acct_wth@
drop procedure p2.acct_trx@
drop procedure p2.add_interest@
--
--  Error Interpretations:
--  1000:    Failed SQL database query
--    -1:    Negative parameter was not accepted
--    -2:    Integer out of bounds error
--    -3:    Invalid non-decimal value
--
CREATE PROCEDURE p2.cust_crt 
(IN name VARCHAR(15), IN gender CHAR, IN age INTEGER, IN pin INTEGER, OUT ID INTEGER, OUT ret_code INTEGER, OUT err_msg VARCHAR(3200))
LANGUAGE SQL
  BEGIN ATOMIC
    DECLARE SQLSTATE CHAR(5) DEFAULT '00000';
    DECLARE SQLCODE INTEGER;
    DECLARE encryptedPin INTEGER;
    DECLARE c CURSOR FOR
      SELECT MAX(ID)
      FROM p2.customer;
    DECLARE CONTINUE HANDLER FOR SQLEXCEPTION, SQLWARNING, NOT FOUND
      BEGIN
        SET ret_code = SQLCODE;

        IF (gender NOT IN ('M', 'F')) THEN
          SET ret_code = -2;
          SET err_msg = 'Gender must be either "M" or "F"';

        ELSEIF (pin < 1) THEN 
          SET ret_code = -2;
          SET err_msg = 'Pin must be greater than 0';

        ELSE
          SET err_msg = SQLERRM (ret_code);

        END IF;
      END;

  SET encryptedPin = p2.encrypt(pin);
  SET ret_code = 0;
  SET err_msg = 'Customer account successfully created!';

  IF age < 0 THEN
    SET ret_code = -1;
    SET err_msg = 'Age cannot be negative';
  ELSE
    INSERT INTO p2.customer ( Name, Gender, Age, Pin ) VALUES ( name, gender, age, encryptedPin );
  
  END IF;

  IF (ret_code = 0) THEN
    OPEN c;
      FETCH c into ID;
    CLOSE c;
  END IF;

END @



CREATE PROCEDURE p2.cust_login
(IN cusId INTEGER, IN cusPin INTEGER, OUT valid BOOLEAN, OUT ret_code INTEGER, OUT err_msg VARCHAR(3200))
LANGUAGE SQL
BEGIN
  DECLARE SQLSTATE CHAR(5) DEFAULT '00000';
  DECLARE SQLCODE INTEGER;
  DECLARE encryptedPin INTEGER;
  DECLARE c CURSOR FOR
    SELECT Pin
    FROM p2.customer
    WHERE ID = cusId AND p2.decrypt(Pin) = cusPin;
  DECLARE CONTINUE HANDLER FOR SQLEXCEPTION, SQLWARNING, NOT FOUND
  BEGIN
    SET ret_code = SQLCODE;
    SET valid = FALSE;

    IF (ret_code = 100) THEN
      SET ret_code = 1000;
      SET err_msg = 'Invalid Customer ID/Pin #';

    ELSE
      SET err_msg = SQLERRM(ret_code);

    END IF;
  END;

  SET ret_code = 0;
  SET err_msg = 'Login Success!';
  SET valid = TRUE;

  OPEN c;
    FETCH c INTO encryptedPin;
  CLOSE c;
END@



CREATE PROCEDURE p2.acct_opn
(IN cusId INTEGER, IN initBalance INTEGER, IN accType CHAR, OUT acctNumber INTEGER, OUT ret_code INTEGER, OUT err_msg VARCHAR(3200))
LANGUAGE SQL
BEGIN ATOMIC
  DECLARE SQLSTATE CHAR(5) DEFAULT '00000';
  DECLARE SQLCODE INTEGER;
  DECLARE c1 CURSOR FOR
    SELECT MAX(Number)
    FROM p2.account;
  DECLARE CONTINUE HANDLER FOR SQLEXCEPTION, SQLWARNING, NOT FOUND
  BEGIN
    SET ret_code = SQLCODE;

    IF (ret_code = -530) THEN
      SET ret_code = 1000;
      SET err_msg = 'Invalid Customer ID';

    ELSE
      SET err_msg = SQLERRM (ret_code);

    END IF;
  END;

  SET ret_code = 0;
  SET err_msg = 'Bank account opened successfully!';

  IF initBalance < 0 THEN
    SET ret_code = -1;
    SET err_msg = 'Initial balance cannot be negative';

  ELSEIF accType NOT IN ('C', 'S') THEN
    SET ret_code = -2;
    SET err_msg = 'Account type must be set to "C" (Checking), or "S" (Savings)';

  ELSE
    INSERT INTO p2.account ( ID, Balance, Type, Status ) VALUES ( cusId, initBalance, accType, 'A' );

  END IF;

  IF (ret_code = 0) THEN
    OPEN c1;
      FETCH c1 INTO acctNumber;
    CLOSE c1;
  END IF;
END@



CREATE PROCEDURE p2.acct_cls
(IN accNo INTEGER, OUT ret_code INTEGER, OUT err_msg VARCHAR(3200))
LANGUAGE SQL
BEGIN ATOMIC
  DECLARE SQLSTATE CHAR(5) DEFAULT '00000';
  DECLARE SQLCODE INTEGER;
  DECLARE accOwnerId INTEGER;
  DECLARE CONTINUE HANDLER FOR SQLEXCEPTION, SQLWARNING, NOT FOUND
  BEGIN
    SET ret_code = SQLCODE;
    
    IF (ret_code = 100) THEN 
      SET ret_code = 1000;
      SET err_msg = 'Invalid account number or account is already inactive';

    ELSE 
      SET err_msg = SQLERRM(ret_code);
    
    END IF;
  END;

  SET ret_code = 0;
  SET err_msg = 'Account successfully closed';

  UPDATE p2.account
  SET Status = 'I',
      Balance = 0
  WHERE Number = accNo AND Status = 'A';

END@



CREATE PROCEDURE p2.acct_dep
(IN accNo INTEGER, IN amount INTEGER, OUT ret_code INTEGER, OUT err_msg VARCHAR(3200))
LANGUAGE SQL
BEGIN
  DECLARE SQLSTATE CHAR(5) DEFAULT '00000';
  DECLARE SQLCODE INTEGER;
  DECLARE CONTINUE HANDLER FOR SQLEXCEPTION, SQLWARNING, NOT FOUND
  BEGIN
    SET ret_code = SQLCODE;

    IF (ret_code = 100) THEN
      SET ret_code = 1000;
      SET err_msg = 'Invalid account number or account closed';

    ELSE
      SET err_msg = SQLERRM(ret_code);
    
    END IF;
  END;

  SET ret_code = 0;
  SET err_msg = 'Deposit Successful';

  IF (amount < 0) THEN
    SET ret_code = -1;
    SET err_msg = 'Amount cannot be negative';

  ELSE
    UPDATE p2.account
    SET Balance = Balance + amount
    WHERE Number = accNo AND Status = 'A';
  
  END IF;
END@



CREATE PROCEDURE p2.acct_wth
(IN accNo INTEGER, IN amount INTEGER, OUT ret_code INTEGER, OUT err_msg VARCHAR(3200))
LANGUAGE SQL
BEGIN
  DECLARE SQLSTATE CHAR(5) DEFAULT '00000';
  DECLARE SQLCODE INTEGER;
  DECLARE currentBalance INTEGER;
  DECLARE c CURSOR FOR
    SELECT Balance
    FROM p2.account
    WHERE Number = accNo AND Status = 'A'
    FOR UPDATE OF Balance;
  DECLARE CONTINUE HANDLER FOR SQLEXCEPTION, SQLWARNING, NOT FOUND
  BEGIN
    SET ret_code = SQLCODE;

    IF (ret_code IN (100, -508)) THEN
      SET ret_code = 1000;
      SET err_msg = 'Invalid account number or account inactive';

    ELSE
      SET err_msg = SQLERRM (ret_code);

    END IF;
  END;

  SET ret_code = 0;
  SET err_msg = 'Withdraw Successful';

  OPEN c;
    FETCH c INTO currentBalance;
      IF (amount < 0) THEN
        SET ret_code = -1;
        SET err_msg = 'Amount cannot be negative';

      ELSEIF (amount > currentBalance) THEN
        SET ret_code = -2;
        SET err_msg = 'Withdraw amount cannot be greater than the current balance';

      ELSE
        UPDATE p2.account
        SET Balance = Balance - amount
        WHERE CURRENT OF c;

      END IF;
  CLOSE c;

END@



CREATE PROCEDURE p2.acct_trx
(IN src_acct INTEGER, IN dest_acct INTEGER, IN amount INTEGER, OUT ret_code INTEGER, OUT err_msg VARCHAR(3200))
LANGUAGE SQL
BEGIN
  DECLARE SQLSTATE CHAR(5) DEFAULT '00000';
  DECLARE SQLCODE INTEGER;
  DECLARE destAccExists INTEGER;
  DECLARE c CURSOR FOR
    SELECT Number
    FROM p2.account
    WHERE Number = dest_acct AND Status = 'A';
  DECLARE CONTINUE HANDLER FOR SQLEXCEPTION, SQLWARNING, NOT FOUND
  BEGIN
    SET ret_code = SQLCODE;

    IF (ret_code = 100) THEN
      SET ret_code = 1000;
      SET err_msg = 'Invalid destination account number, or account closed';

    ELSE
      SET err_msg = SQLERRM (ret_code);

    END IF;
  END;

  SET ret_code = 0;

  OPEN c;
    FETCH c INTO destAccExists;
  CLOSE c;

  IF (ret_code = 0) THEN
    CALL p2.acct_wth(src_acct, amount, ret_code, err_msg);

    IF (ret_code = 0) THEN
      CALL p2.acct_dep(dest_acct, amount, ret_code, err_msg);

      IF (ret_code = 0) THEN
        SET err_msg = 'Transfer Successful';

      END IF;

    ELSEIF (ret_code IN (100, 1000)) THEN
      SET err_msg = 'Invalid source account number, or account closed';

    END IF;

  END IF;
END@



CREATE PROCEDURE p2.add_interest
(IN savings_rate FLOAT, IN checking_rate FLOAT, OUT ret_code INTEGER, OUT err_msg VARCHAR(3200))
LANGUAGE SQL
BEGIN
  DECLARE SQLSTATE CHAR(5) DEFAULT '00000';
  DECLARE SQLCODE INTEGER;
  DECLARE intRate FLOAT;
  DECLARE accNum INTEGER;
  DECLARE accBal INTEGER;
  DECLARE accType CHAR;
  DECLARE done INTEGER DEFAULT 0;
  DECLARE acctCursor CURSOR FOR
    SELECT Number, Balance, Type
    FROM p2.account
    WHERE Status = 'A'
    FOR UPDATE OF Balance;
  DECLARE CONTINUE HANDLER FOR SQLEXCEPTION, SQLWARNING, NOT FOUND
  BEGIN
    IF SQLCODE = -508 THEN
      SET done = 1;
    ELSE 
      SET ret_code = SQLCODE;
      SET err_msg = SQLERRM (ret_code);
    END IF;
  END;

  IF checking_rate < 0 OR savings_rate < 0 THEN
    SET ret_code = -1;
    SET err_msg = 'Interest rates cannot be negative';

  ELSEIF checking_rate > 1 OR savings_rate > 1 THEN
    SET ret_code = -3;
    SET err_msg = 'Interest rates cannot be greater than 1';

  ELSE
    OPEN acctCursor;
      WHILE done = 0 DO
        FETCH acctCursor INTO accNum, accBal, accType;

        IF accType = 'C' THEN
          SET intRate = checking_rate;

        ELSEIF accType = 'S' THEN
          SET intRate = savings_rate;

        END IF;

        UPDATE p2.account
        SET Balance = Balance + (Balance * intRate)
        WHERE CURRENT OF acctCursor;

      END WHILE;

      SET err_msg = 'Interest Applied Successfully';

    CLOSE acctCursor;
  END IF;

END@

terminate@
 