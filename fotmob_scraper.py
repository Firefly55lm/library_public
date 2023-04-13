import nltk
import requests
from bs4 import BeautifulSoup
from nltk import word_tokenize
from urllib.request import urlopen
import re
from tqdm import tqdm

def get_teams(url):
    html = urlopen(url).read()
    soup = BeautifulSoup(html, features="html.parser")
    teams_soup = re.findall('/teams/' + "(.*?)" + '",', str(soup))

    teams = []
    for team in tuple(teams_soup):
        info = str(team).replace("overview/", "")
        if info not in teams:
            teams.append(info)

    return teams

def get_players(team):
    team_id = str(team).split("/")[0]
    team_name = str(team).split("/")[1]
    url = "https://www.fotmob.com/teams/"+ team_id + "/squad/" + team_name

    html = urlopen(url).read()
    soup = BeautifulSoup(html, features="html.parser")

    athletes = re.findall('"athlete":' + "(.*?)" + '}}],"location":{"@type":"Place"', str(soup))
    informations = re.findall('"https://www.fotmob.com/players/' + "(.*?)" + '","nationality":', str(athletes))

    return (team_id, team_name, tuple(informations))

def get_ratings(info, output):
    try:
        url = "https://www.fotmob.com/players/" + str(info)
        name = get_player_name(info)
        id = get_player_id(info)

        html = urlopen(url).read()
        soup = BeautifulSoup(html, features="html.parser")

        stats = soup.find_all("tr", {
            "class": "css-ec7htm-PlayerCareerMatchStatsBodyCSS-playerCareerMatchStatsHeaderCommon e1b3vhfl9"})
        date = re.findall("<td><span>" + "(.*?)" + "</span></td", str(stats))
        ratings = re.findall('"><span>' + "(.*?)" + "</span></div>", str(stats))

        output.append((id, name, tuple(ratings)))

    except:
        pass

def get_player_id(url):
    id = str(url).split("/")[0]
    return id

def get_player_name(url):
    name = str(url).split("/")[1].replace("-", " ").upper()
    return name


if __name__ == "__main__":
    statistics = []

    teams = get_teams("https://www.fotmob.com/leagues/47/overview/premier-league")
    print(f"COLLECTED TEAMS: {teams}")

    team_n = len(teams)
    current = 1

    for team in teams:
        informations = get_players(team)

        team_name = str(team).split('/')[1].upper()
        team_id = str(team).split('/')[0]

        players_ratings = []
        print(f"\n\033[36mGETTING INFORMATIONS ABOUT {team_name} PLAYERS ({current}/{team_n})\033[0m")
        for player in tqdm(informations[2]):
            get_ratings(player, players_ratings)

        statistics.append((team_id, team_name, tuple(players_ratings)))
        current += 1
        print(tuple(players_ratings))


#print("\n", statistics)
