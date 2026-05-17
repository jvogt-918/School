-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='ONLY_FULL_GROUP_BY,STRICT_TRANS_TABLES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION';

-- -----------------------------------------------------
-- Schema mydb
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema mydb
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `mydb` DEFAULT CHARACTER SET utf8mb3 ;
USE `mydb` ;

-- -----------------------------------------------------
-- Table `mydb`.`coaches`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`coaches` (
  `CoachID` INT NOT NULL,
  `CoachFirstName` VARCHAR(45) NULL DEFAULT NULL,
  `CoachLastName` VARCHAR(45) NULL DEFAULT NULL,
  PRIMARY KEY (`CoachID`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb3
COMMENT = '						';


-- -----------------------------------------------------
-- Table `mydb`.`courts`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`courts` (
  `CourtID` INT NOT NULL,
  `CourtLocation` VARCHAR(45) NULL DEFAULT NULL,
  PRIMARY KEY (`CourtID`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb3;


-- -----------------------------------------------------
-- Table `mydb`.`tournaments`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`tournaments` (
  `TourneyID` INT NOT NULL,
  `TourneyDate` DATE NULL DEFAULT NULL,
  `TourneyLocation` VARCHAR(45) NULL DEFAULT NULL,
  PRIMARY KEY (`TourneyID`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb3;


-- -----------------------------------------------------
-- Table `mydb`.`games`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`games` (
  `GamesID` INT NOT NULL,
  `HomeTeamID` VARCHAR(45) NULL DEFAULT NULL,
  `AwayTeamID` VARCHAR(45) NULL DEFAULT NULL,
  `WinningTeamID` VARCHAR(45) NULL DEFAULT NULL,
  `Gamescol` VARCHAR(45) NULL DEFAULT NULL,
  `tournamets_TourneyID` INT NOT NULL,
  `Courts_CourtID` INT NOT NULL,
  PRIMARY KEY (`GamesID`),
  INDEX `fk_Games_tournamets_idx` (`tournamets_TourneyID` ASC) VISIBLE,
  INDEX `fk_Games_Courts1_idx` (`Courts_CourtID` ASC) VISIBLE,
  CONSTRAINT `fk_Games_Courts1`
    FOREIGN KEY (`Courts_CourtID`)
    REFERENCES `mydb`.`courts` (`CourtID`),
  CONSTRAINT `fk_Games_tournamets`
    FOREIGN KEY (`tournamets_TourneyID`)
    REFERENCES `mydb`.`tournaments` (`TourneyID`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb3;


-- -----------------------------------------------------
-- Table `mydb`.`teams`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`teams` (
  `TeamID` INT NOT NULL,
  `TeamName` VARCHAR(45) NULL DEFAULT NULL,
  `Coaches_CoachID` INT NOT NULL,
  PRIMARY KEY (`TeamID`),
  INDEX `fk_Teams_Coaches1_idx` (`Coaches_CoachID` ASC) VISIBLE,
  CONSTRAINT `fk_Teams_Coaches1`
    FOREIGN KEY (`Coaches_CoachID`)
    REFERENCES `mydb`.`coaches` (`CoachID`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb3;


-- -----------------------------------------------------
-- Table `mydb`.`players`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`players` (
  `PlayerID` INT NOT NULL,
  `FirstName` VARCHAR(45) NULL DEFAULT NULL,
  `LastName` VARCHAR(45) NULL DEFAULT NULL,
  `Addresss` VARCHAR(45) NULL DEFAULT NULL,
  `City` VARCHAR(45) NULL DEFAULT NULL,
  `ZipCode` INT NULL DEFAULT NULL,
  `Teams_TeamID` INT NOT NULL,
  PRIMARY KEY (`PlayerID`),
  INDEX `fk_Players_Teams1_idx` (`Teams_TeamID` ASC) VISIBLE,
  CONSTRAINT `fk_Players_Teams1`
    FOREIGN KEY (`Teams_TeamID`)
    REFERENCES `mydb`.`teams` (`TeamID`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb3;


-- -----------------------------------------------------
-- Table `mydb`.`games_has_players`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`games_has_players` (
  `Games_GamesID` INT NOT NULL,
  `Players_PlayerID` INT NOT NULL,
  `PlayerScore` INT NULL DEFAULT NULL,
  PRIMARY KEY (`Games_GamesID`, `Players_PlayerID`),
  INDEX `fk_Games_has_Players_Players1_idx` (`Players_PlayerID` ASC) VISIBLE,
  INDEX `fk_Games_has_Players_Games1_idx` (`Games_GamesID` ASC) VISIBLE,
  CONSTRAINT `fk_Games_has_Players_Games1`
    FOREIGN KEY (`Games_GamesID`)
    REFERENCES `mydb`.`games` (`GamesID`),
  CONSTRAINT `fk_Games_has_Players_Players1`
    FOREIGN KEY (`Players_PlayerID`)
    REFERENCES `mydb`.`players` (`PlayerID`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8mb3;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
