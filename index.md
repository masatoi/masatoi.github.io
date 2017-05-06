---
layout: page
title: masatoi.github.io
tagline: Index
---
{% include JB/setup %}

{% for post in site.posts limit:5 %}

## <a href="{{ post.url }}">{{post.date | date: '%Y-%m-%d'}}: {{ post.title }}</a>

{{ post.content }}

<hr>

{% endfor %}
