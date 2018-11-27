// @flow strict

import * as React from 'react';
import ReactDOM from 'react-dom';
import { BrowserRouter as Router, Route, Link, Switch } from 'react-router-dom';

import Auth from 'Auth';
import type { User } from 'Auth';
import Home from 'components/pages/Home';
import Callback from 'components/pages/Callback';
import Commands from 'components/pages/Commands';
import Logout from 'components/pages/Logout';
import Header from 'components/Header';
import Navbar from 'components/Navbar';

import css from 'main.css';

type State = {|
  dropdownShown: boolean,
|};

class Xanbot extends React.Component<{}> {
  _userHeader() {
    const user = Auth.getUser();

    if (user != null) {
      return <Header user={user} />;
    }
    return null;
  }

  render() {
    return (
      <Router>
        <div className={css.container}>
          <Route component={Navbar} />
          <div className={css.main}>
            <Route path="/" render={() => this._userHeader()} />
            <div className={css.page_content}>
              <Switch>
                <Route path="/" exact component={Home} />
                <Route path="/callback" component={Callback} />
                <Route path="/commands" component={Commands} />
                <Route path="/logout" exact component={Logout} />
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
