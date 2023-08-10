import Image from "next/image";
import {Providers} from "../store/provider";
import React from "react";
import styles from "./explorer.module.css";

// These styles apply to every route in the application
import './global.css'
import 'bootstrap/dist/css/bootstrap.min.css';
import 'bootstrap-icons/font/bootstrap-icons.css';
import Link from "next/link";

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
      <div className={styles.container}>
        <main className={styles.main}>
          <h1 className={styles.title}>
            <Link href="/" className="link-underline-opacity-0 link-body-emphasis ">
              <Image src="/explorer/logo.png" alt="Mithril Logo" width={55} height={55}/> Mithril Explorer
            </Link>
          </h1>
          {children}
        </main>
      </div>
    </Providers>

    <footer className={styles.footer}>
        <span className={styles.logo}>
          <Image src="/explorer/logo.png" alt="Mithril Logo" width={32} height={32}/>
        </span>{' '}
      <a href="https://mithril.network/doc">
        Go back to mithril documentation
      </a>
    </footer>
    </body>
    </html>
  );
}