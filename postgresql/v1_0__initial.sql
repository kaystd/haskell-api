CREATE TABLE users (
    login VARCHAR PRIMARY KEY NOT NULL,
    password VARCHAR NOT NULL,
    user_data VARCHAR NOT NULL,
    key_gost VARCHAR NOT NULL
);

INSERT INTO users (login, password, user_data, key_gost)
VALUES ('Admin', MD5 ('1234'), '', '');
