#! /bin/bash

sed -i 's/<head>/<head>\n      <link rel="shortcut icon" href="data:image\/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20viewBox%3D%220%200%20100%20100%22%3E%3Ctext%20y%3D%22.9em%22%20font-size%3D%2290%22%3E%F0%9F%8C%B1%3C%2Ftext%3E%3C%2Fsvg%3E" type="image\/svg+xml">/' $1
