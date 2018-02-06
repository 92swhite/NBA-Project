from urllib.request import urlopen as uReq
from bs4 import BeautifulSoup as soup
import time

ff = open('possible_fouls.txt','r')
message = ff.read()
ff.close()
possible_fouls = message.split('\n')
for i in possible_fouls:
    if any(char.isdigit() for char in i):
        pass
    else:
        x = possible_fouls.index(i)
        possible_fouls[x] = i.lower()

foul_keywords = ['personal block', 'personal foul', 'personal shooting', 'personal defensive',
                 'personal technical', 'personal blocking', 'personal offensive',
                 'personal holding', 'personal blocking', 'block', 'shooting', 'defensive', 
                 'technical', 'personal', 'blocking', 'flagrant 1', 'flagrant 2', 'take', 'loose ball', 
                 'offensive', 'holding', 'def 3 sec tech', 'flagrant foul type 1',
                 'flagrant foul type 2', 'inbound', 'away from play', 'clear path', 
                 'hanging tech', 'delay tech']

def get_players():
    players_page_soup = soup(page_html_players, "html.parser")
    players_containers = players_page_soup.findAll('div', {'class':'overthrow table_container'})
    away_players = players_containers[0].findAll('th', {'data-stat':'player'})
    del away_players[0]
    del away_players[-1]
    home_players = players_containers[2].findAll('th', {'data-stat':'player'})
    del home_players[0]
    del home_players[-1]
    
    away_players_list = []
    for i in away_players:
        away_players_list.append(i.text)
        
    home_players_list = []
    for i in home_players:
        home_players_list.append(i.text)
    global players_list
    players_list = away_players_list + home_players_list

def foul_type_get(sentence):
    x = []
    for foul in foul_keywords:
        if foul in sentence.lower():
            x.append(foul)
    global foul_type 
    foul_type = max(x)
    
def abb_player_name(players_list):
    global abbNames
    abbNames = []
    for i in players_list:
        try:
            x = i[0] + '.' + ' ' + i.split()[1]
            abbNames.append(x)
        except IndexError:
            continue
    
def who_on_who(sentence):
    global foul_drawn
    global foul_by
    y = []        
    for i in abbNames:    
        if i in sentence:
            y.append(i)
    if len(y) < 2:
        foul_drawn = 'NA'
        if 'team' in sentence.lower():
            foul_by = 'Team'
        else:
            foul_by = y[0]
            for i in players_list:
                if foul_by.split()[1].lower() in i.lower():
                    foul_by = i
    else:
        foul_drawn = 'NA'
    if len(y) == 2:
        if sentence.index(y[0]) > sentence.index(y[1]):
            foul_drawn = y[0]
            foul_by = y[1]
        else:
            foul_drawn = y[1]
            foul_by = y[0]
        for i in players_list:
            if foul_by.split()[1].lower() in i.lower():
                foul_by = i
        for i in players_list:
            if foul_drawn.split()[1].lower() in i.lower():
                foul_drawn = i
        
years_list = ['2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016']

for y in years_list:
    my_url = 'https://www.basketball-reference.com/leagues/NBA_' + y + '_games.html'
    print(my_url)
    uClient = uReq(my_url)
    page_html = uClient.read()
    uClient.close()
    
    page_soup = soup(page_html, "html.parser")
    containers = page_soup.findAll("tr")
    del containers[0]
    container = containers[0]
    
    month_container = page_soup.findAll("div", {"class":"filter"})
    month_container_list = month_container[0].findAll("div")
    month = month_container_list[0].text.strip()
    month_holder = month_container_list[0]
    for a in month_holder.findAll("a", href = True):month_link = (a["href"])
    full_month_link = "https://www.basketball-reference.com" + month_link
    
    for x in month_container_list:
        month = x.text.strip()
        month_holder = x
        for a in month_holder.findAll("a", href = True):month_link = (a["href"])
        full_month_link = "https://www.basketball-reference.com" + month_link
    
        my_url2 = full_month_link
        uClient = uReq(my_url2)
        page_html = uClient.read()
        uClient.close()
        
        page_soup = soup(page_html, "html.parser")
        containers = page_soup.findAll("tr")
        del containers[0]
        container = containers[0]
        
        time.sleep(5)
    
    
        for container in containers:
            
            # containers = entire months table
            # container = one row in that table
            
            box_score_container = container.findAll("td", {"data-stat":"box_score_text"})[0]
            for link in box_score_container:
                foul_page_link = (link.get("href"))
            
            # this gets us the link to the play-by-play page
            full_foul_page_link = "https://www.basketball-reference.com/boxscores/pbp/" + foul_page_link[-17:]
            
            # this makes the pbp page readable
            foul_page_url = full_foul_page_link
            foulClient = uReq(foul_page_url)
            foul_page_html = foulClient.read()
            foulClient.close()
            foul_page_soup = soup(foul_page_html, "html.parser")
            
            # this is the same as above but for players
            my_url_players = 'https://www.basketball-reference.com/boxscores/' + foul_page_link[-17:]
            uClient_players = uReq(my_url_players)
            page_html_players = uClient_players.read()
            uClient_players.close()
            
            players_page_soup = soup(page_html_players, "html.parser")
            players_containers = players_page_soup.findAll('div', {'class':'overthrow table_container'})
            away_players = players_containers[0].findAll('th', {'data-stat':'player'})
            
            # creates a file with the unique name used to find the pbp page
            filename = foul_page_link[-17:-5] + '.csv'
            f = open(filename, "w")
            headers = "date, v_name, v_points, h_name, h_points, ovt, foul_team, foul_type, foul_by, foul_drawn, foul_quarter, foul_score, foul_time\n"
            f.write(headers)
            
            date_container = container.findAll("th", {"data-stat":"date_game"})
            date = date_container[0].text
            
            vistor_container = container.findAll("td", {"data-stat":"visitor_team_name"})
            v_name = vistor_container[0].text
            
            vistor_pts_container = container.findAll("td", {"data-stat":"visitor_pts"})
            v_pts = vistor_pts_container[0].text
            
            home_name_container = container.findAll("td", {"data-stat":"home_team_name"})
            h_name = home_name_container[0].text
            
            home_pts_container = container.findAll("td", {"data-stat":"home_pts"})
            h_pts = home_pts_container[0].text
            
            overtime_container = container.findAll("td", {"data-stat":"overtimes"})
            ovt = overtime_container[0].text
            
            # gives us the table of fouls in pbp page
            table = foul_page_soup.find('table')
            foul_table = table.findAll('tr')
            
            # gives list rows which contain a foul or quarter marker
            foul_list = []
            full_list = []
            for i in foul_table:
                if 'foul' in i.text or 'quarter' in i.text:
                    full_list.append(i.text)
                    
            for i in foul_table:
                if 'foul' in i.text:
                    foul_list.append(i.text)
            
            foul_type = []
            foul_drawn = []
            foul_by = []
            foul_team = []
            foul_score = []
            foul_time = []
            foul_quarter = []
            foul_quarter_holder = []
            
            for i in range(0, len(full_list)):
                if 'quarter' in full_list[i]:
                        foul_quarter_holder.append(i)
        
            for i in range(0, len(foul_list)):
                foul_list_split = foul_list[i].replace('\n', ' ').split()
                foul_by = []
                foul_type = []
                foul_drawn = []
                sentence = foul_list[i].replace('\n', ' ')
                if 'turnover' in sentence.lower():
                    continue    
                get_players()
                foul_type_get(sentence)
                abb_player_name(players_list)
                who_on_who(sentence)
                if any(char.isdigit() for char in foul_list_split[1]):
                    if 'quarter' in foul_list[i]:
                        continue
                    foul_team = v_name
                    foul_score = 'score:' + ' ' + foul_list_split[1]
                else:
                    if 'quarter' in foul_list[i]:
                        continue
                    foul_team = h_name
                    foul_score = 'score:' + ' ' + foul_list_split[-1]
                foul_time = foul_list_split[0][0:4]
                if foul_quarter_holder[0] <= i and i < foul_quarter_holder[1]:
                    foul_quarter = '1st quarter'
                if foul_quarter_holder[2] <= i and i < foul_quarter_holder[3]:
                    foul_quarter = '2nd quarter'
                if foul_quarter_holder[4] <= i and i < foul_quarter_holder[5]:
                    foul_quarter = '3rd quarter'
                if foul_quarter_holder[6] <= i and i < foul_quarter_holder[7]:
                    foul_quarter = '4th quarter'
                try:
                    if foul_quarter_holder[8] <= i and i < foul_quarter_holder[9]:
                        foul_quarter = 'Overtime'
                except IndexError:
                        pass
                        
                f.write(date.replace(",", "") + "," + v_name + "," + v_pts + "," + h_name + "," + h_pts + "," + ovt + ","  +  foul_team + "," + foul_type + "," + foul_by + "," + foul_drawn + "," + foul_quarter + "," + foul_score + "," + foul_time + "," + "\n")   
            
            time.sleep(3)
    
    f.close()