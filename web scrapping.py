from urllib.request import urlopen as uReq
from bs4 import BeautifulSoup as soup
import time

my_url = "https://www.basketball-reference.com/leagues/NBA_2016_games.html"

uClient = uReq(my_url)

page_html = uClient.read()

uClient.close()

page_soup = soup(page_html, "html.parser")
containers = page_soup.findAll("div",{"class":"overthrow table_container"})

# just a single container from containers
container = containers[0]

# Gets month
month = container.table.caption.text

# gets date
date = container.table.tbody.tr.th.text

# also gets start time, but better
start_test = container.findAll("td", {"data-stat":"game_start_time"})
start_time = start_test[0].text

# gets visitor name
vistor_container = container.findAll("td", {"data-stat":"visitor_team_name"})
vistor_container[0].text

# gets visitor points
vistor_pts_container = container.findAll("td", {"data-stat":"visitor_pts"})
vistor_pts_container[0].text

# gets home name
home_name_container = container.findAll("td", {"data-stat":"home_team_name"})
home_name_container[0].text

# gets home points
home_pts_container = container.findAll("td", {"data-stat":"home_pts"})
home_pts_container[0].text

# gets overtime
overtime_container = container.findAll("td", {"data-stat":"overtimes"})
overtime_container[0].text

# gets link to ref page
box_score_container = container.findAll("td", {"data-stat":"box_score_text"})[0]
for link in box_score_container:
    ref_page_link = (link.get("href"))

full_ref_page_link = "https://www.basketball-reference.com" + ref_page_link


# url and stuff for ref page
ref_page_url = full_ref_page_link
refClient = uReq(ref_page_url)
ref_page_html = refClient.read()
refClient.close()
ref_page_soup = soup(ref_page_html, "html.parser")
ref_containers = ref_page_soup.findAll("div",{"class":"overthrow table_container"})

# get ref
ref_container = ref_page_soup.select('a[href*="/referees"]')
ref_container[0].text
ref_container[1].text
ref_container[2].text

filename = "October2016.csv"
f = open(filename, "w")

headers = "date, month, start, v_name, v_points, h_name, h_points, ovt, ref1, ref2, ref3\n"

f.write(headers)

for container in containers:
    date = container.table.tbody.tr.th.text
    month = container.table.caption.text
    
    start_test = container.findAll("td", {"data-stat":"game_start_time"})
    start = start_test[0].text
    
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
    
    box_score_container = container.findAll("td", {"data-stat":"box_score_text"})[0]
    for link in box_score_container:
        ref_page_link = (link.get("href"))

    full_ref_page_link = "https://www.basketball-reference.com" + ref_page_link
    
    time.sleep(5)
    
    ref_page_url = full_ref_page_link
    refClient = uReq(ref_page_url)
    ref_page_html = refClient.read()
    refClient.close()
    ref_page_soup = soup(ref_page_html, "html.parser")
    ref_containers = ref_page_soup.findAll("div",{"class":"overthrow table_container"})
    
    ref_container = ref_page_soup.select('a[href*="/referees"]')
    ref1 = ref_container[0].text
    ref2 = ref_container[1].text
    ref3 = ref_container[2].text
    
    f.write(date + "," + month + "," + start + "," + v_name + "," + v_pts + "," + h_name + "," + h_pts + "," + ovt + "," + ref1 + "," + ref2 + "," + ref3 + "\n")
    
f.close()

