import requests
import re
import os
link = r"http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/"
path_dir = "D:\XXX"
r = requests.get(link)
r.status_code
r.encoding
text = r.text
text = text.encode("ISO-8859-1").decode("UTF-16LE")
l = text.split("\r\n")

#result = re.findall(r'@\w+', 'abc.test@gmail.com, xyz@test.in, test.first@analyticsvidhya.com, first.test@rest.biz')
p = re.compile('href="(?P<link>.+?)"')
x = p.findall(text )
filter_x = [(i, link + i) for i in x if not i.startswith("http")]
print()

def create_file(tupl_):
    name = tupl_[0]
    expansion = name.split(".")[1]
    link = tupl_[1]
    r = requests.get(link)
    if r.status_code != 200:
        #raise Exception("При скачивании файла %s произошла ошибка" % link)
        print('!!!!!! %s' % link)
        return
    if expansion == "pdf":
        print(name, r.encoding)
    else:
        #print("!!!!!!!!!", name, r.encoding)
        #return
        text = r.text
        path_file = os.path.join(path_dir, name)
        try:
            f = open(path_file, encoding="utf-8", mode="w")
            f.write(text)
            print("%s" % name)
        except Exception as e:
            print()
        finally:
            f.close()

for i in filter_x:
    create_file(i)
print()