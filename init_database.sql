CREATE DATABASE IF NOT EXISTS credentials DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci;
USE credentials;

CREATE TABLE IF NOT EXISTS users (
    id INT(20) NOT NULL AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(50) NOT NULL,
    password VARCHAR(255) NOT NULL,
    email VARCHAR(100) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)
ENGINE = InnoDB
DEFAULT CHARSET = utf8
COLLATE = utf8_unicode_ci; -- collation is the set of rules used for comparing characters in a character set (use Unicode for support for non-Latin languages)

-- change the delimiter to // so that we can use ; in the function
DELIMITER //
CREATE FUNCTION check_user_password(username VARCHAR(50), password VARCHAR(255)) RETURNS BOOLEAN
READS SQL DATA
NOT DETERMINISTIC
BEGIN
    DECLARE user_id INT(20);
    DECLARE user_password VARCHAR(255);
    SELECT id, password INTO user_id, user_password FROM users WHERE username = username;

    IF user_id IS NULL THEN
        RETURN FALSE;
    END IF;

    IF user_password = password THEN
        RETURN TRUE;
    END IF;
    
    RETURN FALSE;
END//
DELIMITER ;

-- TODO: Modify the definition from the testing database from procedure to function
DELIMITER //
CREATE FUNCTION add_user(username VARCHAR(50), password VARCHAR(255), email VARCHAR(100)) RETURNS BOOLEAN
MODIFIES SQL DATA
NOT DETERMINISTIC
BEGIN
    DECLARE user_id INT(20);
    SELECT id INTO user_id FROM users WHERE username = username;
    IF user_id IS NOT NULL THEN
        RETURN FALSE;
    END IF;
    INSERT INTO users (username, password, email) VALUES (username, password, email);
    RETURN TRUE;
END//
DELIMITER ;

-- TODO: Modify the definition from the testing database from procedure to function
DELIMITER //
CREATE FUNCTION delete_user(username VARCHAR(50), email VARCHAR(100)) RETURNS BOOLEAN
MODIFIES SQL DATA
NOT DETERMINISTIC
BEGIN
    DECLARE CONTINUE HANDLER FOR SQLEXCEPTION
    BEGIN
        RETURN FALSE;
    END;
    DELETE FROM users WHERE username = username AND email = email;
    RETURN TRUE;
END//
DELIMITER ;