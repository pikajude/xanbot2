// @flow strict

import Cookies from 'js-cookie';

export type User = {
  username: string,
  logo: string,
};

export default {
  getUser(): ?User {
    const sess = Cookies.get('_SESSION');
    if (sess == null) {
      return null;
    }
    const data1 = sess.split('.')[1];
    if (data1 == null) {
      return null;
    }
    const obj = JSON.parse(
      window.atob(data1.replace('-', '+').replace('_', '/'))
    );
    return {
      logo: obj.logo,
      username: obj.sub,
    };
  },

  loggedIn(): boolean {
    return this.getUser() != null;
  },
};
