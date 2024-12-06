temp = readlines("./Day06/example.txt")
# temp = readlines("./inputs/06.txt")

border = length(temp)
temp = [collect(line) for line in temp]
guard_map = Matrix{Char}(undef, border, border)

for i in eachindex(temp)
    for j in 1:length(temp[i])
        guard_map[i, j] = temp[i][j]
    end
end

start = findall(x -> x == '^', guard_map)[1]
pos = start
move = CartesianIndex(-1, 0)

function turn(move)
    CartesianIndex(move[2], -move[1])
end

while true
    # show(stdout, "text/plain", guard_map)
    guard_map[pos] = 'X'
    temp_pos = pos + move

    if guard_map[temp_pos] == '#'
        move = turn(move)
    end

    pos = pos + move
    println(pos)
end

length(findall(x -> x == 'X', guard_map))

function reset_guard_map!()
    for i in 1:border
        for j in 1:border
            if guard_map[i, j] == 'X'
                guard_map[i, j] = '.'
            end
        end
    end
    guard_map[start] = '^'

    nothing
end

reset_guard_map!()

function is_loop(guard_map, O)
    pos = start
    move = CartesianIndex(-1, 0)
    guard_map_copy = copy(guard_map)
    guard_map_copy[O] = '#'

    visited_positions = []

    try
        while true
            current_state = (pos, move)
            # println(current_state)
            # show(stdout, "text/plain", guard_map_copy)
            if any(x -> x == current_state, visited_positions)
                return true
            end

            push!(visited_positions, current_state)

            temp = pos + move

            if guard_map_copy[temp] == '#'
                move = turn(move)
            end

            pos = pos + move
        end
    catch e
        return false
    end
end


empty_positions = findall(x -> x == '.', guard_map)

using Base.Threads
results = zeros(Int, length(empty_positions))
@threads for i in eachindex(empty_positions)
    results[i] = is_loop(guard_map, empty_positions[i])
end
sum(results)
