# Users schema
 
# --- !Ups

CREATE TABLE `user` (
    id         text NOT NULL PRIMARY KEY,
    email      text NOT NULL UNIQUE,
    password   text NOT NULL,
    screenName text NOT NULL,
    permission text NOT NULL
);

# --- !Downs

DROP TABLE `user`;

