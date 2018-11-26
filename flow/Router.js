// @flow strict

import React from 'react';
import { BrowserRouter as Router, Route, Link } from 'react-router-dom';

function AppRouter(props: {}) {
  return (
    <Router>
      <div>
        <Link to="/">Home</Link>

        <Route path="/" exact component={() => <h2>Hello, world!</h2>} />
      </div>
    </Router>
  );
}

type RouteProps = {};

export default AppRouter;
