// @flow strict

import * as React from 'react';
import { Redirect } from 'react-router-dom';
import Cookies from 'js-cookie';

export default class Logout extends React.Component<{}> {
  render() {
    Cookies.remove('_SESSION');
    window.location = '/';
    return null;
  }
}
