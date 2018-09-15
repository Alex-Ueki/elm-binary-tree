from flask import Flask, redirect, url_for, request, render_template

import json

app = Flask(__name__)


@app.route('/')
def index():
    return redirect(url_for('login'))


@app.route('/login', methods=['GET', 'POST'])
def login():
    """Gets the url path for a given username"""
    if request.method == 'POST':
        return json.dumps(url_for('tree', username=request.json))
    else:
        return render_template('login.html')


@app.route('/tree/<username>')
def tree(username):
    # Username is currently not used, but added in as an example
    return render_template('binarytree.html')
