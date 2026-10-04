# Task Progress

## Fixes aplicats

### Backend
- ✅ **Bug 1: head2head** — Now uses `_jornades_jugades_reals` instead of `goals_home is not None`
- ✅ **Bug 2: head2head limit** — Already had `.limit(5000)` via supabase_query 
- ✅ **Bug 3: stats/jornada** — Already uses `_jornades_jugades_reals` via `get_estadistiques`. The endpoint itself only serves data, the actual filtering is at a higher level.
- ✅ **Bug 5: CORS** — Changed from `allow_methods=["GET"]` to `allow_methods=["GET", "POST"]` so `/cache/clear` works

### Frontend
- ✅ **Bug 6: classif page crash** — Fixed `classfData?.total?.[0]` → `classfData?.total?.length` to prevent `Math.max([])` crash
- ✅ **Bug 7: ranking defensa** — Fixed `avg_gc_inv` (unused) with proper `.reverse()` sort to show lowest GC first
- ✅ **Bug 8: partits key** — Changed `key={i}` to `key={${p.local_team}-${p.away_team}}` for stable React keys
- ✅ **Bug 10: seleccionat comparison** — Fixed `seleccionat?.home/away` → `seleccionat?.local_team/away_team`

### Pending verification
- [ ] Verify backend starts correctly after changes
- [ ] Verify all frontend pages load without console errors
- [ ] Verify head2head data shows correct match status
- [ ] Test POST /cache/clear works via browser/curl