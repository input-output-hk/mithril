import React from "react";
import {Providers} from "../store/provider";

// These styles apply to every route in the application
import './global.css'
import 'bootstrap/dist/css/bootstrap.min.css';
import 'bootstrap-icons/font/bootstrap-icons.css';

export const metadata = {
  title: 'Mithril Explorer',
  description: 'Explore a Mithril Network',
}

export default function RootLayout({children}) {
  return (
    <html lang="en">
    <body>
    <link rel="icon" href="/explorer/logo.svg?v=1" type="image/svg+xml"/>
    <Providers>
      {children}
    </Providers>
    </body>
    </html>
  );
}