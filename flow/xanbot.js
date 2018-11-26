// @flow strict

import * as React from 'react';
import ReactDOM from 'react-dom';
import Auth from './Auth';
import type { Payload } from './Auth';
import Home from './pages/Home';
import Callback from './pages/Callback';

import { BrowserRouter as Router, Route, Link } from 'react-router-dom';

import css from '../css/main.css';

type Props = {||};

type State = {|
  user: ?Payload,
|};

class Xanbot extends React.Component<Props, State> {
  _getUser() { return Auth.getUser(); }
  _userHeader() {
    const user = this._getUser();
    if (user != null) {
      return (
        <div className={css.header}>
          <a className={css['header-user']} href="#">
            <img src={user.logo} alt={`${user.username}'s logo`} className={css['header-user-icon']} />
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
          <div className={css.navbar}>
            <Link id={css['home-button']} to="/">&#x1f916;</Link>
          </div>
          <div className={css.main}>
            {this._userHeader()}
            <Route path="/" exact render={props => <Home {...props} loggedIn={this._getUser() != null} />} />
            <Route path="/callback" component={Callback} />
          </div>
        </div>
      </Router>
    );
  }
}

function nullthrows<T>(x: ?T): T {
  if (x == null) {
    throw new Error("Oh noey");
  }
  return x;
}

document.addEventListener("DOMContentLoaded", () => {
  ReactDOM.render(<Xanbot />, nullthrows(document.body));
});
