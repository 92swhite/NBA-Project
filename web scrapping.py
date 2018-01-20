from urllib.request import urlopen as uReq
from bs4 import BeautifulSoup as soup

my_url = "https://www.basketball-reference.com/leagues/NBA_2017_games.html"

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

# url and stuff for ref page
ref_page_url = "https://www.basketball-reference.com/boxscores/201610250CLE.html"
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