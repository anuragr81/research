
# A very simple Flask Hello World app for you to get started with...

from flask import Flask, redirect, url_for, render_template, request

"""


# python anywhere
app = Flask(__name__)
@app.route('/')
def hello_world():
    return 'Hello from Flask!'
"""


def display_image(filename):
    #print('display_image filename: ' + filename)
    return redirect(url_for('static', filename='images/' + filename), code=301)


def query_sanskrit():
    if request.method=="GET":
        request_q=str(request.args.get('q'))
        return "GET:"+request_q
    else:
        return ""
    #return "<html><head/><body> sanskrit request="+request_method+" </body></html>"

def display_sanskrit():
    return render_template('sanskrit.html')

def display_index():
    #dat =""
    #with open('/home/anuragr/research_persona/index.html') as fh:
    #    dat = fh.read()
    #return dat
    return render_template('index.html')


# EB looks for an 'app' callable by default.
app = Flask(__name__)

# add a rule for the index page.
app.add_url_rule('/', 'index', (lambda: display_index()) )
app.add_url_rule('/display/<filename>', view_func=display_image )
app.add_url_rule('/sanskrit/', 'sanskrit', (lambda: display_sanskrit()) )
app.add_url_rule('/query_sanskrit/', 'query_sanskrit', (lambda: query_sanskrit()) )

# add a rule when the page is accessed with a name appended to the site
# URL.
#app.add_url_rule('/<username>', 'hello', (lambda username: header_text + say_hello(username) + home_link + footer_text))

# run the app.
if __name__ == "__main__":
    # Setting debug to True enables debug output. This line should be
    # removed before deploying a production app.
    run_console = False
    if run_console:
        print(display_index())
    else:
        app.debug = False
        app.run()


