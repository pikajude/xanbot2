// @flow strict

import * as React from 'react';
import ReactDOM from 'react-dom';
import Auth from './Auth';
import type { Payload } from './Auth';
import Home from './components/Home';
import Callback from './components/Callback';
import Navbar from './components/Navbar';

import { BrowserRouter as Router, Route, Link, Switch } from 'react-router-dom';

import css from '../css/main.css';

type State = {|
  user: ?Payload,
|};

class Xanbot extends React.Component<{}, State> {
  _userHeader() {
    const user = Auth.getUser();

    if (user != null) {
      return (
        <div className={css.header}>
          <a className={css['header-user']} href="#">
            <img
              src={user.logo}
              alt={`${user.username}'s logo`}
              className={css['header-user-icon']}
            />
            {user.username}
          </a>
        </div>
      );
    }
    return null;
  }

  render() {
    return (
      <Router>
        <div className={css.container}>
          <Route path="/" component={Navbar} />
          <div className={css.main}>
            {this._userHeader()}
            <div className={css.page_content}>
              <Switch>
                <Route path="/" exact component={Home} />
                <Route path="/callback" component={Callback} />
                <Route
                  path="/commands"
                  render={() => <div>Commands page</div>}
                />
              </Switch>
            </div>
          </div>
        </div>
      </Router>
    );
  }
}

function nullthrows<T>(x: ?T): T {
  if (x == null) {
    throw new Error('nullthrows: unexpected null');
  }
  return x;
}

document.addEventListener('DOMContentLoaded', () => {
  ReactDOM.render(<Xanbot />, nullthrows(document.getElementById('page')));
});
