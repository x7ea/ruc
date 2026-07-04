// 使用するには/ProjectRoot/../ViewKit/にViewKitをクローンしてリリースビルドしてください。
// ViewKitはhttps://github.com/mochiOS/ViewKitから入手できます

#include <stdint.h>

#include <viewkit.h>

VkRuntime *ruca_vk_runtime_create(int64_t component_instance_id) {
    return vk_runtime_create((uint64_t)component_instance_id);
}

int64_t ruca_vk_runtime_destroy(VkRuntime *runtime) {
    return (int64_t)vk_runtime_destroy(runtime);
}
