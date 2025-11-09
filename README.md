# Asteroids Game

A classic Asteroids arcade game built with Haskell and Gloss.

Controls

| Key | Action |
|-----|--------|
| W | Thrust forward |
| A | Rotate left |
| D | Rotate right |
| Space | Shoot |
| P | Pause / Resume |
| M | Return to main menu |
| N | New game (from menu) |
| C | View controls (from menu) |

Gameplay

Objective
Survive as long as possible by destroying asteroids and enemies while avoiding collisions.

Scoring
- Small asteroid: 100 points
- Medium asteroid: 50 points
- Large asteroid: 20 points
- Chaser enemy: 200 points
- Shooter enemy: 500 points

Enemy Types
- **Chasers** (spawn at 500 points): Purple ships that hunt you down
- **Shooters** (spawn at 2,500 points): Green ships that fire cyan bullets at you

Game Over
You have 3 lives. When all lives are lost, your score is automatically saved if it's in the top 5.

High Scores

High scores are automatically saved to `highscores.txt` in the game directory. The top 5 scores are tracked across sessions

Credits

Created by Lars Koppelman & Roemer van Gorkum
