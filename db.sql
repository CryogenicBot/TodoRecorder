-- -----------------------------------------------------
-- Schema tdrc
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS tdrc;

SET search_path TO tdrc ;

-- -----------------------------------------------------
-- Table tdrc.users
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS tdrc.users (
  user_id SERIAL,
  email VARCHAR(255) NOT NULL,
  password VARCHAR(255) NOT NULL,
  PRIMARY KEY (user_id));


CREATE UNIQUE INDEX email_UNIQUE ON tdrc.users (email ASC);

CREATE UNIQUE INDEX user_id_UNIQUE ON tdrc.users (user_id ASC);


-- -----------------------------------------------------
-- Table tdrc.movie_records
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS tdrc.movie_records (
  movie_id SERIAL,
  timestamp TIMESTAMP NULL,
  api_id text not null unique,
  PRIMARY KEY (movie_id));


CREATE UNIQUE INDEX movie_id_UNIQUE ON tdrc.movie_records (movie_id ASC);


-- -----------------------------------------------------
-- Table tdrc.exercise_records
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS tdrc.exercise_records (
  exercise_id SERIAL,
  exercise_name VARCHAR(127) NOT NULL,
  sets INT NULL,
  weight INT NULL,
  down_time INT NULL,
  timestamp TIMESTAMP NULL,
  PRIMARY KEY (exercise_id));


CREATE UNIQUE INDEX exercise_id_UNIQUE ON tdrc.exercise_records (exercise_id ASC);


-- -----------------------------------------------------
-- Table tdrc.book_records
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS tdrc.book_records (
  book_id SERIAL,
  timestamp TIMESTAMP NULL,
  api_id text not null unique,
  PRIMARY KEY (book_id));


CREATE UNIQUE INDEX book_id_UNIQUE ON tdrc.book_records (book_id ASC);


-- -----------------------------------------------------
-- Table tdrc.game_records
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS tdrc.game_records (
  game_id SERIAL,
  timestamp TIMESTAMP NULL,
  api_id text not null unique,
  PRIMARY KEY (game_id));


CREATE UNIQUE INDEX game_id_UNIQUE ON tdrc.game_records (game_id ASC);


-- -----------------------------------------------------
-- Table tdrc.user_book_records
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS tdrc.user_book_records (
  user_id INT NOT NULL,
  book_id INT NOT NULL,
  user_book_id SERIAL,
  PRIMARY KEY (user_book_id),
  CONSTRAINT user_id
    FOREIGN KEY (user_id)
    REFERENCES tdrc.users (user_id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT book_id
    FOREIGN KEY (book_id)
    REFERENCES tdrc.book_records (book_id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION);


CREATE UNIQUE INDEX user_book_id_UNIQUE ON tdrc.user_book_records (user_book_id ASC);

CREATE INDEX user_id_idx ON tdrc.user_book_records (user_id ASC);

CREATE INDEX book_id_idx ON tdrc.user_book_records (book_id ASC);


-- -----------------------------------------------------
-- Table tdrc.user_game_records
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS tdrc.user_game_records (
  user_game_id SERIAL,
  user_id INT NOT NULL,
  game_id INT NOT NULL,
  PRIMARY KEY (user_game_id),
  CONSTRAINT user_id
    FOREIGN KEY (user_id)
    REFERENCES tdrc.users (user_id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT game_id
    FOREIGN KEY (game_id)
    REFERENCES tdrc.game_records (game_id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION);


CREATE UNIQUE INDEX user_game_id_UNIQUE ON tdrc.user_game_records (user_game_id ASC);

CREATE INDEX user_id_idx ON tdrc.user_game_records (user_id ASC);

CREATE INDEX game_id_idx ON tdrc.user_game_records (game_id ASC);


-- -----------------------------------------------------
-- Table tdrc.user_exercise_records
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS tdrc.user_exercise_records (
  user_exercise_id SERIAL,
  user_id INT NOT NULL,
  exercise_id INT NOT NULL,
  PRIMARY KEY (user_exercise_id),
  CONSTRAINT user_id
    FOREIGN KEY (user_id)
    REFERENCES tdrc.users (user_id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT exercise_id
    FOREIGN KEY (exercise_id)
    REFERENCES tdrc.exercise_records (exercise_id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION);


CREATE UNIQUE INDEX user_exercise_id_UNIQUE ON tdrc.user_exercise_records (user_exercise_id ASC);

CREATE INDEX user_id_idx ON tdrc.user_exercise_records (user_id ASC);

CREATE INDEX exercise_id_idx ON tdrc.user_exercise_records (exercise_id ASC);


-- -----------------------------------------------------
-- Table tdrc.user_movie_records
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS tdrc.user_movie_records (
  user_movie_id SERIAL,
  user_id INT NOT NULL,
  movie_id INT NOT NULL,
  PRIMARY KEY (user_movie_id),
  CONSTRAINT user_id
    FOREIGN KEY (user_id)
    REFERENCES tdrc.users (user_id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT movie_id
    FOREIGN KEY (movie_id)
    REFERENCES tdrc.movie_records (movie_id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION);


CREATE UNIQUE INDEX user_movie_id_UNIQUE ON tdrc.user_movie_records (user_movie_id ASC);

CREATE INDEX user_id_idx ON tdrc.user_movie_records (user_id ASC);

CREATE INDEX movie_id_idx ON tdrc.user_movie_records (movie_id ASC);
