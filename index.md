---
layout: page
title: Top
tagline: 
---
{% include JB/setup %}

{% for post in site.posts limit:10 %}

## <a href="{{ post.url }}">{{post.date | date: '%Y-%m-%d'}}: {{ post.title }}</a>

{{ post.content }}
{% endfor %}
