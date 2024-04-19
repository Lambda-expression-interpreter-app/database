CREATE DATABASE IF NOT EXISTS credentials DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci;
USE credentials;

CREATE TABLE IF NOT EXISTS users (
    id INT(20) NOT NULL AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(130) NOT NULL,
    password VARCHAR(130) NOT NULL,
    email VARCHAR(130) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)
ENGINE = InnoDB
DEFAULT CHARSET = utf8
COLLATE = utf8_unicode_ci; -- collation is the set of rules used for comparing characters in a character set (use Unicode for support for non-Latin languages)

-- change the delimiter to // so that we can use ; in the function
DELIMITER //
CREATE FUNCTION check_user_password(p_username VARCHAR(130), p_password VARCHAR(130)) RETURNS BOOLEAN
READS SQL DATA
NOT DETERMINISTIC
BEGIN
    DECLARE user_id INT(20);
    DECLARE user_password VARCHAR(255);
    SELECT id, password INTO user_id, user_password FROM users WHERE username = p_username;

    IF user_id IS NULL THEN
        RETURN FALSE;
    END IF;

    IF user_password = p_password THEN
        RETURN TRUE;
    END IF;
    
    RETURN FALSE;
END//
DELIMITER ;

DELIMITER //
CREATE PROCEDURE add_user(IN p_username VARCHAR(130), IN p_password VARCHAR(130), IN p_email VARCHAR(130))
BEGIN
    DECLARE user_id INT(20);
    SELECT id INTO user_id FROM users WHERE username = p_username;
    IF user_id IS NOT NULL THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'User already exists';
    END IF;
    INSERT INTO users (username, password, email) VALUES (p_username, p_password, p_email);
END //
DELIMITER ;

DELIMITER //
CREATE PROCEDURE delete_user(IN p_username VARCHAR(130), IN p_email VARCHAR(130))
BEGIN
    DELETE FROM users WHERE username = p_username AND email = p_email;
END//
DELIMITER ;